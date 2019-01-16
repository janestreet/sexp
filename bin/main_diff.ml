open Core
open Async


let load file ~expand_macros =
  if expand_macros
  then Sexp_macro.Blocking.load_sexp file |> return
  else Reader.file_contents file >>| Sexp.of_string
;;

let expand_macros_flag =
  let open Command.Param in
  flag "expand-macros" no_arg ~doc:" expand macros in input files"
;;

module Display_function = struct
  type t =
    { display_options : Sexp_diff_kernel.Display.Display_options.t
    ; display_as_plain_string : bool
    }

  let create ?(display_as_plain_string = false) ?collapse_threshold ?num_shown () =
    let display_options =
      Sexp_diff_kernel.Display.Display_options.create ?collapse_threshold ?num_shown ()
    in
    { display_options; display_as_plain_string }
  ;;

  let flags =
    let open Command.Let_syntax in
    let%map_open () = return ()
    and display_as_plain_string =
      flag "plain" no_arg ~doc:" display as plain string instead of ANSI colors"
    and collapse_threshold =
      flag
        "collapse-threshold"
        (optional int)
        ~doc:"INT set [collapse_threshold] in Display_options (default: 10)"
    and num_shown =
      flag
        "num-shown"
        (optional int)
        ~doc:"INT set [num_shown] in Display_options (default: 3)"
    in
    match display_as_plain_string, collapse_threshold, num_shown with
    | false, None, None -> None
    | _ -> Some (create ~display_as_plain_string ?collapse_threshold ?num_shown ())
  ;;

  let run { display_options; display_as_plain_string } diff =
    let display =
      if display_as_plain_string
      then Sexp_diff_kernel.Display.display_as_plain_string
      else Sexp_diff_kernel.Display.display_with_ansi_colors
    in
    display ~display_options diff |> Core.print_endline
  ;;
end

module Diff_mode = struct
  type t =
    | Emit_patch
    | Emit_diff of Display_function.t
    | Quiet

  let flags =
    let open Command.Param in
    choose_one
      ~if_nothing_chosen:(`Default_to (Emit_diff (Display_function.create ())))
      [ flag
          "for-patch"
          no_arg
          ~doc:" print the sexp representation of the diff for use with \"sexp patch\""
        |> map ~f:(fun x -> Option.some_if x Emit_patch)
      ; flag "quiet" no_arg ~doc:" print nothing. run only for the exit code"
        |> map ~f:(fun x -> Option.some_if x Quiet)
      ; Display_function.flags |> map ~f:(Option.map ~f:(fun x -> Emit_diff x))
      ]
  ;;
end

let diff_command =
  Command.async
    ~summary:"print the diff of two sexp files (expanding macros)"
    ~readme:(fun () ->
      String.strip
        {|
Whenever there is a sequence of [collapse_threshold] or more unchanged lines, we collapse
them and only show the first [num_shown] and the last [num_shown] of these lines.

Like the unix diff utility, this command exits non-zero (with exit code 2) when its inputs
are different, so it can be used as a sexp equality test with the -quiet flag.  Passing
the -for-patch flag disables this behavior.

It also exists non-zero (with exit code 1) if either sexp is malformed.
|})
    (let open Command.Let_syntax in
     let%map_open file1, file2 =
       anon (t2 ("FILE1" %: Filename.arg_type) ("FILE2" %: Filename.arg_type))
     and mode = Diff_mode.flags
     and expand_macros = expand_macros_flag in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind original = load file1 ~expand_macros in
       let%bind updated = load file2 ~expand_macros in
       let diff = Sexp_diff_kernel.Algo.diff ~original ~updated () in
       (* emit output *)
       (match mode with
        | Quiet -> ()
        | Emit_patch -> Sexp_diff_kernel.Diff.sexp_of_t diff |> print_s
        | Emit_diff display_fun -> Display_function.run display_fun diff);
       (* similar to "diff", we exit non-zero if the files are different *)
       match (diff : Sexp_diff_kernel.Diff.t) with
       | Same _ -> return ()
       | Add _ | Delete _ | Replace _ | Enclose _ ->
         (match mode with
          | Emit_patch ->
            (* In this case, we're running for the patch output, not the comparison *)
            return ()
          | Emit_diff _ | Quiet -> exit 2))
;;

let patch_command =
  Command.async
    ~summary:"apply a diff to a sexp file (expanding macros)"
    ~readme:(fun () ->
      String.strip
        {|
The resulting sexp is printed to stdout.

DIFF-FILE should have the same format as that produced by [sexp diff -for-patch].
|})
    (let open Command.Let_syntax in
     let%map_open diff_file, file =
       anon
         (t2
            ("DIFF-FILE" %: Filename.arg_type)
            (maybe_with_default "/dev/stdin" ("FILE" %: Filename.arg_type)))
     and expand_macros = expand_macros_flag in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind diff =
         load diff_file ~expand_macros >>| Sexp_diff_kernel.Diff.t_of_sexp
       in
       let%map file = load file ~expand_macros in
       Sexp_diff_kernel.Diff.apply_exn diff file |> print_s)
;;

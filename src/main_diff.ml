open Core
open Async

let load file ~expand_macros ~multiple_sexps_in_each_file =
  if expand_macros
  then Sexp_macro.Blocking.load_sexp file |> return
  else if multiple_sexps_in_each_file
  then Reader.file_contents file >>| sprintf "( %s )" >>| Sexp.of_string
  else Reader.file_contents file >>| Sexp.of_string
;;

let expand_macros_flag =
  let open Command.Param in
  flag "expand-macros" no_arg ~doc:" expand macros in input files"
;;

let multiple_sexps_in_each_file_flag =
  let open Command.Param in
  flag
    "multiple-sexps-in-each-file"
    no_arg
    ~doc:" multiple sexps in each input file (incompatible with -expand-macros)"
;;

module Display_function = struct
  type t =
    { display_options : Sexp_diff.Display.Display_options.t
    ; display_as_plain_string : bool
    }

  let default_layout = Sexp_diff.Display.Display_options.Layout.Two_column

  let create
    ?(display_as_plain_string = false)
    ?collapse_threshold
    ?(layout = default_layout)
    ?num_shown
    ()
    =
    let display_options =
      Sexp_diff.Display.Display_options.create ?collapse_threshold ?num_shown layout
    in
    { display_options; display_as_plain_string }
  ;;

  let flags =
    let%map_open.Command () = return ()
    and display_as_plain_string =
      flag "plain" no_arg ~doc:" display as plain string instead of ANSI colors"
    and collapse_threshold =
      flag
        "collapse-threshold"
        (optional int)
        ~doc:"INT set [collapse_threshold] in Display_options (default: 10)"
    and layout =
      let module Layout = Sexp_diff.Display.Display_options.Layout in
      Enum.make_param_optional_one_of_flags (module Layout) ~doc:(fun layout ->
        match [%compare.equal: Layout.t] layout default_layout with
        | false -> ""
        | true -> "(default)")
    and num_shown =
      flag
        "num-shown"
        (optional int)
        ~doc:"INT set [num_shown] in Display_options (default: 3)"
    in
    match display_as_plain_string, collapse_threshold, num_shown with
    | false, None, None -> None
    | _ ->
      Some (create ~display_as_plain_string ?collapse_threshold ?layout ?num_shown ())
  ;;

  let run { display_options; display_as_plain_string } diff =
    let display =
      if display_as_plain_string
      then Sexp_diff.Display.display_as_plain_string
      else Sexp_diff.Display.display_with_ansi_colors
    in
    display display_options diff |> Core.print_endline
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
      ~if_nothing_chosen:(Default_to (Emit_diff (Display_function.create ())))
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
      {|
Whenever there is a sequence of [collapse_threshold] or more unchanged lines, we collapse
them and only show the first [num_shown] and the last [num_shown] of these lines.

Like the unix diff utility, this command exits non-zero (with exit code 2) when its inputs
are different, so it can be used as a sexp equality test with the -quiet flag.  Passing
the -for-patch flag disables this behavior.

It also exists non-zero (with exit code 1) if either sexp is malformed.
|})
    (let%map_open.Command file1, file2 =
       anon (t2 ("FILE1" %: Filename_unix.arg_type) ("FILE2" %: Filename_unix.arg_type))
     and mode = Diff_mode.flags
     and expand_macros = expand_macros_flag
     and multiple_sexps_in_each_file = multiple_sexps_in_each_file_flag in
     fun () ->
       if expand_macros && multiple_sexps_in_each_file
       then failwith "Incompatible flags -expand-macros and -multiple-sexps-in-each-file";
       let%bind original = load file1 ~expand_macros ~multiple_sexps_in_each_file in
       let%bind updated = load file2 ~expand_macros ~multiple_sexps_in_each_file in
       let diff = Sexp_diff.Algo.diff ~original ~updated () in
       (* emit output *)
       (match mode with
        | Quiet -> ()
        | Emit_patch -> Sexp_diff.Diff.sexp_of_t diff |> print_s
        | Emit_diff display_fun -> Display_function.run display_fun diff);
       (* similar to "diff", we exit non-zero if the files are different *)
       match (diff : Sexp_diff.Diff.t) with
       | Same _ -> return ()
       | Add _ | Delete _ | Replace _ | Enclose _ ->
         (match mode with
          | Emit_patch ->
            (* In this case, we're running for the patch output, not the comparison *)
            return ()
          | Emit_diff _ | Quiet -> exit 2))
    ~behave_nicely_in_pipeline:false
;;

let patch_command =
  Command.async
    ~summary:"apply a diff to a sexp file (expanding macros)"
    ~readme:(fun () ->
      {|
The resulting sexp is printed to stdout.

DIFF-FILE should have the same format as that produced by [sexp diff -for-patch].
|})
    (let%map_open.Command diff_file, file =
       anon
         (t2
            ("DIFF-FILE" %: Filename_unix.arg_type)
            (maybe_with_default "/dev/stdin" ("FILE" %: Filename_unix.arg_type)))
     and expand_macros = expand_macros_flag in
     fun () ->
       let%bind diff =
         load diff_file ~expand_macros ~multiple_sexps_in_each_file:false
         >>| Sexp_diff.Diff.t_of_sexp
       in
       let%map file = load file ~expand_macros ~multiple_sexps_in_each_file:false in
       Sexp_diff.Diff.apply_exn diff file |> print_s)
    ~behave_nicely_in_pipeline:false
;;

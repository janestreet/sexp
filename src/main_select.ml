open Core
open Async

let example_sexp_string =
  {|
    ((foo bar)
     (baz (
       (sausage banana)
       (fred    george)
       (wizzle (
         (one   a)
         (two   b)
         (three c)))))
     (wizzle fizzle)
     (wizzle (
       (grizzle (
         (one z)
         (two y)))
       (drizzle chizzle)))
     (fred percy))
  |}
;;

let example_programs =
  [ "foo"
  ; "sausage"
  ; "fred"
  ; "baz fred"
  ; "two"
  ; "wizzle two"
  ; "wizzle > two"
  ; "wizzle > ( one two )"
  ; "wizzle ( one two )"
  ; "wizzle"
  ; "> wizzle"
  ; "wizzle > *"
  ]
;;

let readme () =
  let example_sexp = Parsexp.Single.parse_string_exn example_sexp_string in
  let example_programs_and_output =
    Sexp_select.format_program_outputs example_sexp example_programs
  in
  String.strip
    {|
Implementation of a subset of CSS-style selectors for traversing sexp trees.

See also the get and query subcommands.

Syntax:
- "foo" finds the value of every pair in your tree having "foo" as the key
- "foo bar" finds the value of every pair in your tree having "foo" as the
  key, and then for each of these trees finds the value of every pair having
  "bar" as the key.
- "foo > bar" finds the value of every pair in your tree having "foo" as the
  key, and then for each of these trees finds the value of every top-level pair
  having "bar" as the key.
- "foo > ( bar baz )" finds the value of every pair in your tree having "foo" as the
  key, and then for each of these trees finds the value of every top-level pair
  having either "bar" or "baz" as the key.
- "*" matches anything

Examples:
|}
  ^ sprintf "\n%s\n\n%s" example_sexp_string example_programs_and_output
;;

let readme_flag () =
  let open Command.Param in
  flag
    "readme"
    (no_arg_abort ~exit:(fun () ->
       Core.print_endline (readme ());
       Core.exit 0))
    ~doc:" Show the readme"
;;

let remove_duplicates_flag =
  let open Command.Param in
  flag
    ~doc:" remove duplicate outputs from each PROGRAM"
    "remove-dupes"
    (map_flag no_arg ~f:(fun arg ->
       if arg then List.stable_dedup ~compare:Sexp.compare else Fn.id))
;;

let mach_flag =
  let open Command.Param in
  map Shared_params.machine ~f:(fun mach ->
    if mach then Sexp.to_string_mach else fun sexp -> Sexp.to_string_hum sexp)
;;

let command =
  Command.async
    ~summary:"Use CSS-style selectors to traverse sexp trees"
    (let%map_open.Command () = readme_flag ()
     and program = anon ("program" %: string)
     and sexp_to_string = mach_flag
     and maybe_sexp_string = anon (maybe ("sexp" %: string))
     and maybe_remove_duplicate_outputs = remove_duplicates_flag in
     fun () ->
       let sexp_pipe =
         match maybe_sexp_string with
         | None -> Reader.read_sexps (Lazy.force Reader.stdin)
         | Some x -> Pipe.singleton (Sexp.of_string x)
       in
       let select_fn = unstage (Sexp_select.select_staged program) in
       Pipe.iter_without_pushback sexp_pipe ~f:(fun sexp ->
         List.iter
           (maybe_remove_duplicate_outputs (select_fn sexp))
           ~f:(fun answer -> printf "%s\n%!" (sexp_to_string answer))))
;;

let multi_command =
  Command.async
    ~summary:
      "like [sexp select], but allowing multiple programs to be passed, and grouping \
       together output from each input sexp"
    (let%map_open.Command () = readme_flag ()
     and labeled =
       flag "labeled" no_arg ~doc:" label each match with the PROGRAM that matched it"
     and sexp_to_string = mach_flag
     and maybe_remove_duplicate_outputs = remove_duplicates_flag
     and programs =
       map
         ~f:(fun (x, xs) -> x :: xs)
         (anon (non_empty_sequence_as_pair ("program" %: string)))
     in
     fun () ->
       let programs =
         List.map programs ~f:(fun program ->
           program, unstage (Sexp_select.select_staged program))
       in
       Reader.read_sexps (Lazy.force Reader.stdin)
       |> Pipe.iter_without_pushback ~f:(fun sexp ->
         match
           List.concat_map programs ~f:(fun (program, select_fn) ->
             List.map
               (maybe_remove_duplicate_outputs (select_fn sexp))
               ~f:(fun answer ->
                 if labeled
                 then [%sexp_of: string * Sexp.t] (program, answer)
                 else answer))
         with
         | [] -> ()
         | sexps -> printf "%s\n%!" (sexp_to_string ([%sexp_of: Sexp.t list] sexps))))
;;

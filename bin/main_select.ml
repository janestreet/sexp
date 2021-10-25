open Core
open Async

module Test_and_doc = struct
  let test_sexp_string =
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

  let test_sexp = Sexp.of_string test_sexp_string

  let tests =
    [ "foo", [ "bar" ]
    ; "sausage", [ "banana" ]
    ; "fred", [ "george"; "percy" ]
    ; "baz fred", [ "george" ]
    ; "two", [ "b"; "y" ]
    ; "wizzle two", [ "b"; "y" ]
    ; "wizzle > two", [ "b" ]
    ; "wizzle > ( one two )", [ "a"; "b" ]
    ; "wizzle ( one two )", [ "a"; "b"; "z"; "y" ]
    ; ( "wizzle"
      , [ "((one a)(two b)(three c))"
        ; "fizzle"
        ; "((grizzle ((one z)(two y)))(drizzle chizzle))"
        ] )
    ; "wizzle > *", [ "a"; "b"; "c"; "((one z)(two y))"; "chizzle" ]
    ]
  ;;

  (* can't do TEST_UNIT b/c this isn't a library *)
  let () =
    List.iter tests ~f:(fun (program, expected) ->
      [%test_result: Sexp.t list]
        (Sexp_select.select program test_sexp)
        ~expect:(List.map ~f:Sexp.of_string expected))
  ;;

  let doc () =
    let max_program_width =
      List.fold tests ~init:0 ~f:(fun prev_max (program, _) ->
        Int.max (String.length program) prev_max)
    in
    let pad s = s ^ String.make (max_program_width - String.length s) ' ' in
    let output_padding = String.make (max_program_width + String.length " -> [ ") ' ' in
    List.map tests ~f:(fun (program, output) ->
      let output_str =
        let one_line = String.concat ~sep:"; " output in
        if String.length one_line <= 40
        then one_line
        else
          (match output with
           | [] -> assert false
           | head :: tail -> head :: List.map tail ~f:(fun s -> output_padding ^ s))
          |> String.concat ~sep:";\n"
      in
      Printf.sprintf "%s -> [ %s ]" (pad program) output_str)
    |> String.concat ~sep:"\n"
  ;;

  let readme =
    String.strip
      {|
"Implementation of a subset of CSS-style selectors for traversing sexp trees.

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
    ^ sprintf "\n%s\n\n%s" test_sexp_string (doc ())
  ;;

  let readme_flag () =
    let open Command.Param in
    flag
      "readme"
      (no_arg_abort ~exit:(fun () ->
         Core.print_endline readme;
         Core.exit 0))
      ~doc:" Show the readme"
  ;;
end

let remove_duplicates_flag =
  let open Command.Param in
  flag
    ~doc:" remove duplicate outputs from each PROGRAM"
    "remove-dupes"
    (map_flag no_arg ~f:(fun arg -> if arg then Sexp.Set.stable_dedup_list else Fn.id))
;;

let mach_flag =
  let open Command.Param in
  flag
    ~doc:" print machine-style sexp output"
    "machine"
    (map_flag no_arg ~f:(fun mach ->
       if mach then Sexp.to_string_mach else fun sexp -> Sexp.to_string_hum sexp))
;;

let command =
  Command.async
    ~summary:"Use CSS-style selectors to traverse sexp trees"
    (let open Command.Let_syntax in
     let%map_open () = Test_and_doc.readme_flag ()
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
       Pipe.iter_without_pushback sexp_pipe ~f:(fun sexp ->
         List.iter
           (maybe_remove_duplicate_outputs (Sexp_select.select program sexp))
           ~f:(fun answer -> printf "%s\n%!" (sexp_to_string answer))))
;;

let multi_command =
  Command.async
    ~summary:
      "like [sexp select], but allowing multiple programs to be passed, and grouping \
       together output from each input sexp"
    (let open Command.Let_syntax in
     let%map_open () = Test_and_doc.readme_flag ()
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
       Reader.read_sexps (Lazy.force Reader.stdin)
       |> Pipe.iter_without_pushback ~f:(fun sexp ->
         match
           List.concat_map programs ~f:(fun program ->
             List.map
               (maybe_remove_duplicate_outputs (Sexp_select.select program sexp))
               ~f:(fun answer ->
                 if labeled
                 then [%sexp_of: string * Sexp.t] (program, answer)
                 else answer))
         with
         | [] -> ()
         | sexps -> printf "%s\n%!" (sexp_to_string ([%sexp_of: Sexp.t list] sexps))))
;;

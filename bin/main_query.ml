open Core
open Sexp_app

type source =
  | Anon of Syntax.query
  | File of string
  | Script of string

let syntax_error msg e = failwithf "Syntax error: %s\n\t%s" msg (Exn.to_string e) ()

let load' f x ~is_change =
  let t_of_sexp =
    if is_change
    then fun sexp -> Syntax.Change (Syntax.Change.t_of_sexp sexp)
    else Syntax.Query.t_of_sexp
  in
  let sexps =
    try f x with
    | e -> syntax_error "bad s-expression" e
  in
  try Syntax.pipe (List.map ~f:t_of_sexp sexps) with
  | e -> syntax_error "bad program" e
;;

let load = load' ~is_change:false

let load_file file ~is_change ~skip_first_line =
  let handle = In_channel.create file in
  if skip_first_line then ignore (In_channel.input_line_exn handle : string);
  load' Sexp.input_sexps handle ~is_change
;;

let create_perform_query_f ~source ~is_change =
  let prgm =
    match source with
    | Anon prgm -> prgm
    | File file -> load_file file ~skip_first_line:false ~is_change
    | Script file -> load_file file ~skip_first_line:true ~is_change
  in
  fun sexp_ext ~on_result ->
    let lazy_results = Semantics.query' prgm sexp_ext in
    Lazy_list.iter lazy_results ~f:on_result
;;

let query_arg =
  Command.Arg_type.create (fun prgm -> load Sexp.scan_sexps @@ Lexing.from_string prgm)
;;

let query_command =
  Command.basic
    ~summary:"query an s-expression"
    ~readme:(fun () -> Grammar.grammar_for_readme ())
    (let%map_open.Command () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            print_endline Readme.query_by_example_dot_md;
            exit 0))
         ~doc:" Detailed, example-driven guide to using sexp-query"
     and () =
       flag
         "formal-semantics"
         (no_arg_abort ~exit:(fun () ->
            print_endline Readme.query_semantics_dot_md;
            exit 0))
         ~doc:" Show a doc describing sexp-query's formal semantics"
     and () =
       flag
         "grammar"
         (no_arg_abort ~exit:(fun () ->
            Grammar.print ();
            exit 0))
         ~doc:" Show full grammar for sexpquery programs"
     and () =
       flag
         "quine"
         (no_arg_abort ~exit:(fun () ->
            Quine.show stdout;
            exit 0))
         ~doc:" Print a sexp-query quine (outputs itself on any input sexp)"
     and source, inputs, labeled_default =
       let%map_open.Command file =
         flag
           "file"
           (optional Filename_unix.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       and script =
         flag
           "script"
           (optional Filename_unix.arg_type)
           ~doc:"FILE Read program from file instead of command line (skip #!)"
       and query =
         anon
           (maybe
              (t2 ("QUERY" %: query_arg) (sequence ("FILE" %: Filename_unix.arg_type))))
       and stdin_label =
         flag
           "stdin-label"
           (optional string)
           ~doc:"LABEL override default label for stdin"
       in
       (* switch from command line parsing to argument processing *)
       let query, files =
         match query with
         | None -> None, []
         | Some (query, files) -> Some query, files
       in
       let inputs, labeled_default =
         match files with
         | [] -> Located.stdin stdin_label, Option.is_some stdin_label
         | [ file ] -> Located.files [ file ], false
         | files -> Located.files files, true
       in
       let source =
         match file, script, query with
         | Some x, None, None -> File x
         | None, Some x, None -> Script x
         | None, None, Some x -> Anon x
         | _ -> failwith "must pass exactly one of QUERY, -file, and -script"
       in
       source, inputs, labeled_default
     and group =
       flag
         "group"
         no_arg
         ~doc:" Group incoming sequence of sexps into a single list sexp"
     and { machine; fail_on_parse_error } = Shared_params.machine_and_fail_on_parse_error
     and { output_mode; allow_empty_output; labeled } = Shared_params.query_args in
     fun () ->
       let perform_query = create_perform_query_f ~source ~is_change:false in
       Query.execute
         { inputs
         ; output_mode
         ; allow_empty_output
         ; group
         ; machine
         ; labeled = Option.value labeled ~default:labeled_default
         ; fail_on_parse_error
         ; perform_query
         })
;;

let change_arg = Command.Param.sexp_conv Syntax.Change.t_of_sexp

let stdin_label_arg =
  let open Command.Param in
  flag "stdin-label" (optional string) ~doc:"LABEL override default label for stdin"
;;

let create_inputs ~files ~stdin_label =
  match files with
  | [] -> Located.stdin stdin_label, Option.is_some stdin_label
  | [ file ] -> Located.files [ file ], false
  | files -> Located.files files, true
;;

let change_command_body ~files ~stdin_label ~source ~machine ~fail_on_parse_error =
  let inputs, labeled = create_inputs ~files ~stdin_label in
  let perform_query = create_perform_query_f ~source ~is_change:true in
  Query.execute
    { inputs
    ; output_mode = Sexp
    ; allow_empty_output = false
    ; group = false
    ; machine
    ; labeled
    ; fail_on_parse_error
    ; perform_query
    }
;;

let change_command =
  Command.basic
    ~summary:"transform an s-expression"
    (let%map_open.Command () =
       flag
         "formal-semantics"
         (no_arg_abort ~exit:(fun () ->
            print_endline Readme.change_semantics_dot_md;
            exit 0))
         ~doc:" Documentation dump"
     and () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            print_endline Readme.change_by_example_dot_md;
            exit 0))
         ~doc:" Show examples of change expressions"
     and () =
       flag
         "grammar"
         (no_arg_abort ~exit:(fun () ->
            Grammar.print ();
            exit 0))
         ~doc:" Show grammar for change expressions"
     and { machine; fail_on_parse_error } = Shared_params.machine_and_fail_on_parse_error
     and source, files =
       let%map_open.Command x =
         anon
           (maybe
              (t2 ("QUERY" %: change_arg) (sequence ("FILE" %: Filename_unix.arg_type))))
       and file =
         flag
           "file"
           (optional Filename_unix.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       in
       match x, file with
       | None, Some x -> File x, []
       | Some (prgm, files), None -> Anon (Syntax.Change prgm), files
       | _ -> failwith "must pass exactly one of -file and QUERY"
     and stdin_label = stdin_label_arg in
     fun () ->
       change_command_body ~files ~stdin_label ~source ~machine ~fail_on_parse_error)
;;

let pattern_arg = Command.Param.sexp_conv Syntax.Pattern.t_of_sexp

let rewrite_command =
  Command.basic
    ~summary:"rewrite patterns within an s-expression"
    ~readme:(fun () ->
      {|
rewrite patterns within an s-expression

  main.exe rewrite A B [FILE ...]

is exactly the same as

  sexp change '(topdown (try (rewrite A B)))'

but easier to remember, find, and use.

Say we have this sexp:

$ cat /tmp/sexp
((laundry true)
(basket
  (fruit   Banana)
  (utensil fork)
  (utensil knife)
  (also
    (fruit Pear)
    (fruit Lychee))))

We can rewrite the "fruit" fields to "snack" quite easily:

$ cat /tmp/sexp | sexp rewrite '(fruit $FRUIT)' '(snack $FRUIT)'
((laundry true)
(basket
  (snack   Banana)
  (utensil fork)
  (utensil knife)
  (also
    (snack Pear)
    (snack Lychee))))


See `sexp change -examples` for more information.|})
    (let%map_open.Command { machine; fail_on_parse_error } =
       Shared_params.machine_and_fail_on_parse_error
     and source_a = anon ("A" %: pattern_arg)
     and source_b = anon ("B" %: pattern_arg)
     and files = anon (sequence ("FILE" %: Filename_unix.arg_type))
     and stdin_label = stdin_label_arg in
     fun () ->
       change_command_body
         ~files
         ~stdin_label
         ~machine
         ~fail_on_parse_error
         ~source:(Anon (Change (Topdown (Syntax.try_ (Rewrite (source_a, source_b)))))))
;;

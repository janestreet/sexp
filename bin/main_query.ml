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
    (let open Command.Let_syntax in
     let%map_open () =
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
       let%map_open file =
         flag
           "file"
           (optional Filename.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       and script =
         flag
           "script"
           (optional Filename.arg_type)
           ~doc:"FILE Read program from file instead of command line (skip #!)"
       and query =
         anon (maybe (t2 ("QUERY" %: query_arg) (sequence ("FILE" %: Filename.arg_type))))
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
     and machine =
       flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
     and output_mode =
       let%map_open quiet =
         flag
           "quiet"
           no_arg
           ~doc:" Produce no output (useful when running for exit status alone)"
       and count = flag "count" no_arg ~doc:" Produce only a count of returned sexps" in
       match quiet, count with
       | true, false -> Query.Silent
       | false, true -> Query.Count
       | false, false -> Query.Sexp
       | true, true -> failwith "can't pass both -quiet and -count"
     and allow_empty_output =
       flag "allow-empty-output" no_arg ~doc:" Do not fail even if no match is found"
     and labeled =
       let%map_open label =
         flag "label" no_arg ~doc:" pair with filenames (override default behavior)"
       and no_label =
         flag
           "no-label"
           no_arg
           ~doc:" do not pair with filenames (override default behavior)"
       in
       match label, no_label with
       | true, false -> Some true
       | false, true -> Some false
       | false, false -> None
       | true, true -> failwith "can't pass both -label and -no-label flags"
     and fail_on_parse_error =
       flag
         "fail-on-parse-error"
         no_arg
         ~doc:" raise exception on bad input (override default behavior)"
     in
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

let change_command =
  Command.basic
    ~summary:"transform an s-expression"
    (let open Command.Let_syntax in
     let%map_open () =
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
     and machine =
       flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
     and fail_on_parse_error =
       flag
         "fail-on-parse-error"
         no_arg
         ~doc:" raise exception on bad input (override default behavior)"
     and source, files =
       let%map_open x =
         anon
           (maybe (t2 ("QUERY" %: change_arg) (sequence ("FILE" %: Filename.arg_type))))
       and file =
         flag
           "file"
           (optional Filename.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       in
       match x, file with
       | None, Some x -> File x, []
       | Some (prgm, files), None -> Anon (Syntax.Change prgm), files
       | _ -> failwith "must pass exactly one of -file and QUERY"
     and stdin_label =
       flag "stdin-label" (optional string) ~doc:"LABEL override default label for stdin"
     in
     fun () ->
       let inputs, labeled =
         match files with
         | [] -> Located.stdin stdin_label, Option.is_some stdin_label
         | [ file ] -> Located.files [ file ], false
         | files -> Located.files files, true
       in
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
         })
;;

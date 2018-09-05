open Core

let query_arg =
  Command.Arg_type.create (fun prgm ->
    Query.load Sexp.scan_sexps @@ Lexing.from_string prgm)
;;

let command =
  Command.basic
    ~summary:"query an s-expression"
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
         ~doc:" Show grammar for sexpquery programs"
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
         | Some x, None, None -> Query.File x
         | None, Some x, None -> Query.Script x
         | None, None, Some x -> Query.Anon x
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
       Query.main
         { source
         ; inputs
         ; output_mode
         ; allow_empty_output
         ; group
         ; machine
         ; labeled = Option.value labeled ~default:labeled_default
         ; fail_on_parse_error
         })
;;

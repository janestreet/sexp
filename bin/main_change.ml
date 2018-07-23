open Core
open Sexp_app

let change_arg = Command.Param.sexp_conv Syntax.Change.t_of_sexp

let command =
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
         anon (maybe (t2 ("QUERY" %: change_arg) (sequence ("FILE" %: file))))
       and file =
         flag
           "file"
           (optional file)
           ~doc:"FILE Read program from file instead of command line"
       in
       match x, file with
       | None, Some x -> Query.File x, []
       | Some (prgm, files), None -> Query.Anon (Syntax.Change prgm), files
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
       Query.main_change
         { source
         ; inputs
         ; output_mode = Query.Sexp
         ; allow_empty_output = false
         ; group = false
         ; machine
         ; labeled
         ; fail_on_parse_error
         })
;;

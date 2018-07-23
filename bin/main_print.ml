open Core

let command =
  Command.basic
    ~summary:"pretty-print an s-expression"
    (let open Command.Let_syntax in
     let%map_open machine =
       flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
     and fail_on_parse_error =
       flag
         "fail-on-parse-error"
         no_arg
         ~doc:" raise exception on bad input (override default behavior)"
     in
     fun () ->
       Query.main
         { source = Query.Anon Sexp_app.Syntax.This
         ; inputs = Located.stdin None
         ; output_mode = Query.Sexp
         ; allow_empty_output = true
         ; group = false
         ; machine
         ; labeled = false
         ; fail_on_parse_error
         })
;;

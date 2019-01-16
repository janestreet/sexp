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
       let perform_query sexp_ext ~on_result =
         let lazy_results = Sexp_app.Semantics.query' Sexp_app.Syntax.This sexp_ext in
         Lazy_list.iter lazy_results ~f:on_result
       in
       Query.execute
         { inputs = Located.stdin None
         ; output_mode = Query.Sexp
         ; allow_empty_output = true
         ; group = false
         ; machine
         ; labeled = false
         ; fail_on_parse_error
         ; perform_query
         })
;;

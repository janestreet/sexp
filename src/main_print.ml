open Core

let command =
  Command.basic
    ~summary:"pretty-print an s-expression"
    (let%map_open.Command { machine; fail_on_parse_error } =
       Shared_params.machine_and_fail_on_parse_error
     in
     fun () ->
       let perform_query sexp ~on_result = on_result sexp in
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

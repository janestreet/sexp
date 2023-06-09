open Core

let execute query =
  try Query.execute query with
  | _ -> exit 1
;;

(* preserve failure, but silently *)

let command =
  Command.basic
    ~summary:"validate s-expressions from stdin or one or more files"
    (let%map_open.Command files = anon (sequence ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       let perform_query sexp_ext ~on_result =
         let lazy_results = Sexp_app.Semantics.query' Sexp_app.Syntax.This sexp_ext in
         Lazy_list.iter lazy_results ~f:on_result
       in
       execute
         { inputs =
             (match files with
              | [] -> Located.stdin None
              | files -> Located.files files)
         ; output_mode = Query.Silent
         ; allow_empty_output = true
         ; group = false
         ; machine = true
         ; labeled = false
         ; fail_on_parse_error = true
         ; perform_query
         })
;;

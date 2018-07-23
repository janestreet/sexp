open Core

let main query = try Query.main query with _ -> exit 1

(* preserve failure, but silently *)

let command =
  Command.basic
    ~summary:"validate a sequence of s-expressions on stdin"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       main
         { source = Query.Anon Sexp_app.Syntax.This
         ; inputs = Located.stdin None
         ; output_mode = Query.Silent
         ; allow_empty_output = true
         ; group = false
         ; machine = true
         ; labeled = false
         ; fail_on_parse_error = true
         })
;;

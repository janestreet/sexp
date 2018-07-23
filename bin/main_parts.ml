open Core
open Sexp_app

let rec flatten () =
  match try Some (Sexp.input_sexp In_channel.stdin) with End_of_file -> None with
  | None -> ()
  | Some sexp ->
    Parts.output (Parts.flatten sexp) stdout;
    flatten ()
;;

let rec assemble () =
  let flattened = Parts.input In_channel.stdin in
  if flattened = []
  then ()
  else (
    Sexp.output_mach stdout (Parts.assemble flattened);
    print_endline "";
    assemble ())
;;

let flatten_command =
  Command.basic
    ~summary:"Flatten a list of sexp into its parts.  Each part on its own line."
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> flatten ())
;;

let assemble_command =
  Command.basic
    ~summary:
      "Assemble a lists of parts into sexps.  Sexp part lists are separated by newlines."
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> assemble ())
;;

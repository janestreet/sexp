open Core
module Sexp = Sexplib.Sexp

let main infile cout =
  List.iter (Sexp_macro.Blocking.load_sexps infile) ~f:(fun sexp ->
    Sexp.output_hum cout sexp;
    (* a whitespace is necessary to separate adjacent atoms *)
    Out_channel.output_char cout '\n')
;;

let readme () =
  "Resolve a sexp with macros as understood by [Sexp_macros].\n\
   Reads from INFILE and writes to stdout (or OUTFILE)"
;;

let command =
  Command.basic
    ~summary:"resolve macros in a sexp"
    ~readme
    (let open Command.Let_syntax in
     let%map_open infile, maybe_cout =
       anon
         (t2
            ("INFILE" %: Filename_unix.arg_type)
            (maybe ("OUTFILE" %: Filename_unix.arg_type)))
     in
     fun () ->
       let cout = stdout in
       (* defaults *)
       match maybe_cout with
       | None -> main infile cout
       | Some outfile -> Out_channel.with_file outfile ~f:(fun cout -> main infile cout))
;;

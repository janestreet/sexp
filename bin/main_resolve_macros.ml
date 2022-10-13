open Core
module Sexp = Sexplib.Sexp

module Mode = struct
  type t =
    | Print_resolved_sexp
    | Print_included_files

  let flags =
    let%map_open.Command only_print_included_files =
      flag
        "only-print-loaded-files"
        no_arg
        ~doc:
          "Instead of printing the resolved macros, only print the names of the files \
           that are loaded as a result of resolving the input file (including the input \
           file itself)."
    in
    if only_print_included_files then Print_included_files else Print_resolved_sexp
  ;;
end

let main mode infile cout =
  match (mode : Mode.t) with
  | Print_resolved_sexp ->
    List.iter (Sexp_macro.Blocking.load_sexps infile) ~f:(fun sexp ->
      Sexp.output_hum cout sexp;
      (* a whitespace is necessary to separate adjacent atoms *)
      Out_channel.output_char cout '\n')
  | Print_included_files ->
    Sexp_macro.Blocking.included_files infile |> Out_channel.output_lines cout
;;

let readme () =
  "Resolve a sexp with macros as understood by [Sexp_macros].\n\
   Reads from INFILE and writes to stdout (or OUTFILE)"
;;

let command =
  Command.basic
    ~summary:"resolve macros in a sexp"
    ~readme
    (let%map_open.Command mode = Mode.flags
     and infile, maybe_cout =
       anon
         (t2
            ("INFILE" %: Filename_unix.arg_type)
            (maybe ("OUTFILE" %: Filename_unix.arg_type)))
     in
     fun () ->
       let k cout = main mode infile cout in
       match maybe_cout with
       | None -> k stdout
       | Some outfile -> Out_channel.with_file outfile ~f:k)
;;

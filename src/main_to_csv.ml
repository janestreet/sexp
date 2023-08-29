open Core
open Sexp_app

let myprotect f x =
  try Some (f x) with
  | _ -> None
;;

let main ~in_channel ~two_pass_processing ~view_atoms_as_strings ~delimiter:sep =
  let sexps =
    let channel = Lexing.from_channel in_channel in
    Lazy_list.build ~seed:() ~f:(fun () ->
      match myprotect Sexp.scan_sexp channel with
      | None -> None
      | Some sexp -> Some (sexp, ()))
  in
  Csv_file.write
    Out_channel.stdout
    (To_csv.csv_of_sexp ~two_pass_processing ~view_atoms_as_strings sexps)
    ~sep
;;

let command =
  Command.basic
    ~summary:
      "Converts a list of record s-expressions from stdin or a file into CSV format."
    ~readme:(fun () ->
      {|
Example

    COMMAND

        sexp to-csv <INPUT >OUTPUT

    INPUT

        ((foo a) (bar 2) (baz 8))
        ((foo b) (bar 3) (baz 88))
        ((foo c) (bar 4) (baz 888))

    OUTPUT

        foo,bar,baz
        a,2,8
        b,3,88
        c,4,888
|})
    (let%map_open.Command view_atoms_as_sexps =
       flag
         "atoms-as-sexps"
         no_arg
         ~doc:
           " when the extracted CSV value is an atom, dump it as a string rather than as \
            an s-expression (Note: this causes atoms with embedded whitespace to show up \
            triple-quoted)"
     and two_pass_processing =
       flag
         "two-pass-processing"
         no_arg
         ~doc:
           " Uses one pass the gather all columns names for the header and a second to \
            generate the rows. This ensures that no data will be discarded from any \
            record, but requires more memory. Without this option, the header is \
            generated solely by the first record. Any field not found in the first \
            record---but found in later records---will be dropped."
     and delimiter =
       flag
         "delimiter"
         (optional_with_default ',' char)
         ~doc:(sprintf "CHAR use this delimiter instead of ','")
     and in_channel = Shared_params.channel_stdin_or_anon_file in
     fun () ->
       main
         ~in_channel
         ~delimiter
         ~two_pass_processing
         ~view_atoms_as_strings:(not view_atoms_as_sexps))
;;

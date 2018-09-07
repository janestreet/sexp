open Core
open Sexp_app

let myprotect f x =
  try Some (f x) with
  | _ -> None
;;

let main ~view_atoms_as_strings ~delimiter:sep =
  let sexps =
    let channel = Lexing.from_channel In_channel.stdin in
    Lazy_list.build ~seed:() ~f:(fun () ->
      match myprotect Sexp.scan_sexp channel with
      | None -> None
      | Some sexp -> Some (sexp, ()))
  in
  Csv_file.write
    Out_channel.stdout
    (To_csv.csv_of_sexp ~view_atoms_as_strings sexps)
    ~sep
;;

let command =
  Command.basic
    ~summary:"converts a list of record s-expressions into CSV format"
    ~readme:(fun () ->
      String.strip
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
    (let open Command.Let_syntax in
     let%map_open view_atoms_as_sexps =
       flag
         "atoms-as-sexps"
         no_arg
         ~doc:
           " when the extracted CSV value is an atom, dump it as a string rather than as \
            an s-expression (Note: this causes atoms with embedded whitespace to show up \
            triple-quoted)"
     and delimiter =
       flag
         "delimiter"
         (optional_with_default ',' char)
         ~doc:(sprintf "CHAR use this delimiter instead of ','")
     in
     fun () -> main ~delimiter ~view_atoms_as_strings:(not view_atoms_as_sexps))
;;

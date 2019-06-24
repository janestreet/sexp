open Core
module Sexp = Sexplib.Sexp

let rec restructure = function
  | Sexp.List xs -> Sexp.List (List.map ~f:restructure xs)
  | Sexp.Atom foo as same ->
    (try
       assert (Sexplib.Pre_sexp.must_escape foo && String.length foo > 80);
       let xs = Sexp.scan_sexps (Lexing.from_string foo) in
       let xs = List.map ~f:restructure xs in
       Sexp.List (Sexp.Atom "-RESTRUCTURED-" :: xs)
     with
     | _ -> same)
;;

let main cin cout = Sexp.output_hum cout (restructure (Sexp.input_sexp cin))

let readme () =
  "Attempt to recover the structure in an s-expression containing atoms\n\
   constructed by some use of Sexp.to_string.  For example, this often\n\
   happens with error messages with deep structure.  Reads from stdin (or INFILE)\n\
   and writes to stdout (or OUTFILE)"
;;

let command =
  Command.basic
    ~summary:"recover structure of an s-expression"
    ~readme
    (let open Command.Let_syntax in
     let%map_open argv =
       anon
         (maybe
            (t2 ("INFILE" %: Filename.arg_type) (maybe ("OUTFILE" %: Filename.arg_type))))
     in
     fun () ->
       let cin, cout = In_channel.stdin, Out_channel.stdout in
       (* defaults *)
       match argv with
       | None -> main cin cout
       | Some (infile, outfile) ->
         In_channel.with_file infile ~f:(fun cin ->
           match outfile with
           | None -> main cin cout
           | Some outfile ->
             Out_channel.with_file outfile ~f:(fun cout -> main cin cout)))
;;

open Core
open Async
module Sexp_path = Sexplib.Path

let main ~ignore_errors ~sexp_paths () =
  Reader.read_sexps (Lazy.force Reader.stdin)
  |> Pipe.iter_without_pushback ~f:(fun sexp ->
    let strings =
      List.map (String.split sexp_paths ~on:',') ~f:(fun sexp_path ->
        let ignore_errors, sexp_path =
          if String.(slice sexp_path (-1) 0 = "?")
          then true, String.slice sexp_path 0 (-1)
          else ignore_errors, sexp_path
        in
        try
          let path = Sexp_path.parse sexp_path in
          let res = Sexp_path.get ~path sexp in
          match res with
          | Sexp.Atom s -> s
          | Sexp.List l ->
            (try
               let res = List.map ~f:Sexp.to_string_mach l in
               String.concat ~sep:" " res
             with
             | _ -> Sexp.to_string_hum res)
        with
        | exn -> if ignore_errors then "" else raise exn)
    in
    print_endline (String.concat ~sep:"," strings))
;;

let command =
  let readme () =
    String.strip
      {|
See also the query and select subcommands.

Path syntax:

  "."           -> separates path elements; must be present at start of string.
  "[N]"         -> specifies the Nth element in a tuple.
  "some_tag[N]" -> tries to match [some_tag], then denotes its Nth argument.
  "name"        -> denotes record field having [name].
  "...?"        -> ignore errors for this path only.

Examples for sexp "((a 23) (b 24) (c (2 3 5)))":

  ".a"        -> "23"
  ".a,.b"     -> "23,24"
  ".c"        -> "2 3 5"
  ".[1]"      -> "b 24"
  ".[2]"      -> "c (2 3 5)"
  ".[2].c[0]" -> "2 3 5"
  ".c.[1]"    -> "3"
|}
  in
  Command.async
    ~summary:"extract parts of an s-expression"
    ~readme
    (let%map_open.Command ignore_errors =
       flag "ignore-errors" no_arg ~doc:" ignore errors and print whatever is found"
     and sexp_paths = anon ("\"SEXP-PATH(,SEXP-PATH)*\"" %: string) in
     fun () -> main ~ignore_errors ~sexp_paths ())
    ~behave_nicely_in_pipeline:false
;;

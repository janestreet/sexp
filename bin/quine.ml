open Core
open Sexp_app

let prgm =
  String.strip
    {|
(pipe
  (quote (
    quote (pipe (unquote (wrap (cat (quote quote) this))) (unquote this))))
  (quote (pipe (unquote (wrap (cat (quote quote) this))) (unquote this))))
|}
;;

let prgm_sexp = Sexp.of_string prgm
let prgm = Syntax.Query.t_of_sexp prgm_sexp

let show chan =
  Sexp.output_hum chan prgm_sexp;
  print_endline ""
;;

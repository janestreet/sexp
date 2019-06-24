open! Core

(* The purpose of this is to make it so that 'sexp print' has the option to make a 'best
   effort' at parsing anything rather than failing. It's frustrating when working in the
   console to have a big dump of data that contains sexps but isn't itself a sexp, and
   then attempt to pipe it to 'sexp print' and having sexp print fail because the extra
   stuff causes it to fail to parse. *)
(* Turns an in_channel -> lexbuf by fixing up any problems in the input such that the sexp
   parser should be able to parse everything in the lexbuf. This also causes any comments
   in the sexp to get interpreted directly as part of the sexp rather than omitted. *)

val lexbuf_of_channel : In_channel.t -> Lexing.lexbuf

(* Does the same thing directly to a string *)

val transform_string : string -> string

(* Incremental version *)

val read_of_next_char
  :  next_char:(unit -> char option)
  -> (unit -> [ `Ok of string | `Eof ]) Staged.t

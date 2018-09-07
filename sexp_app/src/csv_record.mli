(** CSV records *)

type t = string list

val read : Lexing.lexbuf -> t option
val write : ?sep:char -> out_channel -> t -> unit

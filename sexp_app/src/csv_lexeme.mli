(** This module encapsulates the lexical structure of the CSV format In particular, it
    does everything necessary to handle double-quotes. *)

module T : sig
  type t =
    | Field of string
    | Comma
    | Newline
end

type t = T.t

val read : Lexing.lexbuf -> t option
val write : ?sep:char -> out_channel -> t -> unit

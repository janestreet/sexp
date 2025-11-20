(* an asymtotically good incremental string concatenater *)

type t

val empty : t
val add : t -> string -> t
val singleton : string -> t
val add_char : t -> char -> t
val dump : t -> string

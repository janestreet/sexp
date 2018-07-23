open! Core

type t = (Sexplib.Path.t * Sexp.t) list

val flatten : Sexp.t -> t

val assemble : t -> Sexp.t

val input : In_channel.t -> t

val output : t -> Out_channel.t -> unit

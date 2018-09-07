open! Core

type t = Sexp.t Hash_set.t

val create : unit -> t
val of_list : Sexp.t list -> t

include Sexpable with type t := t

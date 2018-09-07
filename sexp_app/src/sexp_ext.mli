open! Core

type t =
  | Atom of string
  | List of t Lazy_list.t

include Sexpable with type t := t

val equal : Sexp.t -> t -> bool
val lowercase : t -> t
val sub_expressions : t -> t Lazy_list.t

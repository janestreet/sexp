open! Core
open Syntax

type 'sexp query_meaning = 'sexp -> Sexp.t Lazy_list.t

val query : Query.t -> Sexp.t query_meaning
val query' : Query.t -> Sexp_ext.t query_meaning
val change : Change.t -> Sexp.t -> Sexp.t option

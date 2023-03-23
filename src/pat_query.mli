(** using [sexp pat-query] style queries to extract keys for sorting *)

open! Core

val run : Sexp_app_pattern.Query.t -> (Sexp.t -> Sexp.t list) Staged.t

open! Core
open Sexp_app

type source =
  | Anon of Syntax.query
  | File of string
  | Script of string

type output_mode =
  | Sexp
  | Count
  | Silent

type t =
  { source : source
  ; inputs : unit Located.t
  ; output_mode : output_mode
  ; allow_empty_output : bool
  ; labeled : bool
  ; group : bool
  ; machine : bool
  ; fail_on_parse_error : bool
  }

val load : ('a -> Sexp.t list) -> 'a -> Syntax.query

val main : t -> unit

(* For parsing File/Script sources as changes *)

val main_change : t -> unit

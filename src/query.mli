open! Core
open Sexp_app

type output_mode =
  | Sexp
  | Count
  | Silent

type t =
  { inputs : unit Located.t
  ; output_mode : output_mode
  ; allow_empty_output : bool
  ; labeled : bool
  ; group : bool
  ; machine : bool
  ; fail_on_parse_error : bool
  ; perform_query : Sexp_ext.t -> on_result:(Sexp.t -> unit) -> unit
  }

val execute : t -> unit

open! Base

(** An output specifies how the user wants their captured results to be formatted,
    by providing a sexp structure in which the captures will get embedded. *)
type t =
  | Atom of string
  | Capture of string
  | List of t list
[@@deriving sexp]

val all_captures : t -> string list
val embed_captures : t -> f:(string -> Sexp.t) -> Sexp.t

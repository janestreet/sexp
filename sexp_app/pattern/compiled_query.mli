open! Core

(** Same as Query.t, except that Re2 regexes have been compiled and captures
    have been boiled down into simply an index into an array where that capture should be
    stored. *)
type t =
  | Capture of t * int
  | Any
  | Atom of string
  | Atom_regex of Re2.t
  | Sequence of t list
  | Star of t
  | Plus of t
  | Maybe of t
  | List of t
  | Set of t list
  | Subsearch of t
  | And of t list
  | Or_shortcircuiting of t list
  | Or_all of t list
  | First_match_only of t
[@@deriving sexp_of]

(** Compiles a query. Returns [t, `Labels_of_captures labels, output_method].

    [labels] is the list of all unique keys that expect to receive a capture, a
    stringified integer in the case of a numbered capture, the string name in the case of
    a named capture, and unique default-created labels in the case of captures that the
    user did not themselves label.

    The array indices of [labels] correspond one-to-one with the integers returned in
    the [Capture] variant of [t].

    [output_method] is the method to be used for postprocessing and returning captures. *)
val create
  :  Query.t
  -> 'output_type Output_method.Desired.t
  -> t * [ `Labels_of_captures of string array ] * 'output_type Output_method.Compiled.t

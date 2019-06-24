open! Core

(** Same as Query_types.Query, except that Re2 regexes have been compiled and captures
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

val create
  :  Query.t
  -> Output.t option
  -> t
     * [ `Names_of_captures of string array ]
     * [ `Output of Output.t
       | `Output_as_list
       | `Output_as_record
       | `Output_single_capture
       ]

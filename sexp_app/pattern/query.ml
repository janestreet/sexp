open! Core

type t =
  | Capture_unlabeled of t
  | Capture_to_number of int * t
  | Capture_to_field of string * t
  | Any
  | Atom of string
  | Atom_regex of string
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
[@@deriving sexp]

let rec iter t ~f =
  f t;
  match t with
  | Any | Atom _ | Atom_regex _ -> ()
  | Capture_unlabeled sub
  | Capture_to_number (_, sub)
  | Capture_to_field (_, sub)
  | Subsearch sub
  | First_match_only sub
  | Star sub
  | Plus sub
  | Maybe sub
  | List sub -> iter sub ~f
  | Sequence subs
  | And subs
  | Or_shortcircuiting subs
  | Or_all subs
  | Set subs -> List.iter subs ~f:(fun sub -> iter sub ~f)
;;

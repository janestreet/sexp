open! Core

module Set_kind = struct
  type t =
    { optional : bool
    ; first_only : bool
    }
  [@@deriving sexp]
end

type t =
  | Capture_unlabeled of t
  | Capture_to_number of int * t
  | Capture_to_name of string * t
  | Any
  | Atom of string
  | Atom_regex of string
  | Sequence of t list
  | Star of t
  | Star_greedy of t
  | Plus of t
  | Plus_greedy of t
  | Maybe of t
  | Maybe_greedy of t
  | List of t
  | Set of (t * Set_kind.t) list
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
  | Capture_to_name (_, sub)
  | Subsearch sub
  | First_match_only sub
  | Star sub
  | Star_greedy sub
  | Plus sub
  | Plus_greedy sub
  | Maybe sub
  | Maybe_greedy sub
  | List sub -> iter sub ~f
  | Sequence subs | And subs | Or_shortcircuiting subs | Or_all subs ->
    List.iter subs ~f:(fun sub -> iter sub ~f)
  | Set subs -> List.iter subs ~f:(fun (sub, _) -> iter sub ~f)
;;

module Capture_count = struct
  type t =
    { num_number_captures : int
    ; num_named_captures : int
    ; num_unlabeled_captures : int
    }
end

let count_captures t =
  let num_number_captures = ref 0 in
  let num_named_captures = ref 0 in
  let num_unlabeled_captures = ref 0 in
  iter t ~f:(function
    | Capture_unlabeled _ -> incr num_unlabeled_captures
    | Capture_to_number _ -> incr num_number_captures
    | Capture_to_name _ -> incr num_named_captures
    | Any
    | Atom _
    | Atom_regex _
    | Subsearch _
    | First_match_only _
    | Star _
    | Star_greedy _
    | Plus _
    | Plus_greedy _
    | Maybe _
    | Maybe_greedy _
    | List _
    | Sequence _
    | And _
    | Or_shortcircuiting _
    | Or_all _
    | Set _ -> ());
  { Capture_count.num_number_captures = !num_number_captures
  ; num_named_captures = !num_named_captures
  ; num_unlabeled_captures = !num_unlabeled_captures
  }
;;

let validate_all_captures_labeled_or_all_unlabeled_exn t =
  let { Capture_count.num_number_captures; num_named_captures; num_unlabeled_captures } =
    count_captures t
  in
  if num_unlabeled_captures > 0 && (num_number_captures > 0 || num_named_captures > 0)
  then
    failwith
      "Cannot mix unlabeled captures with named or numbered captures in the same pattern"
;;

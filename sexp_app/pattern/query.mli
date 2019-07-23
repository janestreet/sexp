open! Core

(**
   A query is a object that when applied to a sexp list, may fail to match, or else may
   match by successfully consuming zero or more elements off of the head of that list.

   Queries may match with multiplicity greater than 1, each match independently consuming
   the same or different prefixes of the head of the list.

   Each match may also capture also capture various subsexplists of the consumed sexps,
   returning those captures to the user.

   Captures do NOT need to be uniquely numbered or named within a given query, since with
   the [Or_*] variants, one can legitimately write branched queries where each branch
   might capture into the same label, but only one branch at a time is expected to
   actually match.

   However, if a given single match of the pattern captures two different expressions into
   the same label simultaneously, it is unspecified which of the two expressions
   will actually be returned for that label.
*)
type t =
  (* All apply [t] and for each match, also captures the sexps that [t] consumes.
     Capture_unlabeled - indexes the resulting capture with an incrementing integer.
     Capture_to_number - indexes the resulting capture with the specified integer.
     Capture_to_named - indexes the resulting capture with the specified name. *)
  | Capture_unlabeled of t
  | Capture_to_number of int * t
  | Capture_to_name of string * t
  (* Consume a single sexp if possible *)
  | Any
  (* Consumes an atom if it matches this exact string *)
  | Atom of string
  (* Consumes an atom if it matches this regex *)
  | Atom_regex of string
  (* Recursively applies each [t] against the remaining tail unconsumed by previous [t]s
     in the sequence. *)
  | Sequence of t list
  (* Iteratively applies all of [ Sequence []; Sequence [t]; Sequence [t;t] ... ], with
     a special-case guard against an infinite loop if [t] itself can consume zero
     elements. *)
  | Star of t
  (* Iteratively applies all of [ Sequence [t]; Sequence [t;t] ... ]. *)
  | Plus of t
  (* Iteratively applies both of [ Sequence []; Sequence [t] ], with a special-case
     guard to match with multiplicity only 1 if [t] itself can consume zero elements *)
  | Maybe of t
  (* Consumes a sexp if it is a list, applying [t] to the sublist and requiring any
     successful matches by [t] to consume all elements of the sublist *)
  | List of t
  (* Consumes a sexp if it is a list, recursively applying each subquery [t] iteratively
     one by one to each element of the sublist where that element is wrapped as a
     singleton list. Recursion on elements is WITH-replacement, and the cartesian
     product of all possible matches over the subqueries will be the result. *)
  | Set of t list
  (* Consumes any number of sexps, iteratively applying [t] to all sublists of the
     consumed sexps or any of their subsexps. Additionally consumes and discards
     any number of additional sexps. *)
  | Subsearch of t
  (* Consumes a number of sexps only if every [t] provided has a successful match
     consuming exactly that same number of sexps. *)
  | And of t list
  (* Applies each [t] iteratively, stopping after iterating through all matches of the
     first [t] that has any matches *)
  | Or_shortcircuiting of t list
  (* Applies each [t] iteratively, matching according to the union of their matches *)
  | Or_all of t list
  (* Applies [t] and stops after the first time it matches *)
  | First_match_only of t
[@@deriving sexp]

val iter : t -> f:(t -> unit) -> unit

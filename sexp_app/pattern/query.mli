open! Core

(** This module contains the type for a regexp-like query which can be used to match sexps
    and extract subsexps. *)

(** A {e sexp sequence} is a list of sexps. We use the term sequence to differentiate from
    a sexp which is a list of sexps. Typically a sexp sequence corresponds to a sublist of
    a sexp list.

    A {e query} is a object that when applied to a sexp sequence, may fail to match, or
    else may match by successfully consuming zero or more elements from the head of that
    sequence.

    Queries may match with multiplicity greater than 1, each match independently consuming
    the same or different prefixes of the head of the list.

    Each match may also capture various sequences (which must be a subsequence of the
    consumed sexp sequences) returning those captures to the user.

    Captures do {e not} need to be uniquely numbered or named within a given query, since
    with the [Or_*] variants, one can legitimately write branched queries where each
    branch might capture into the same label, but only one branch at a time is expected to
    actually match.

    However, if a given single match of the pattern captures two different expressions
    into the same label simultaneously, it is unspecified which of the two expressions
    will actually be returned for that label. *)

module Set_kind : sig
  (** Specifies how a term in a [Set] should be matched *)
  type t =
    { optional : bool
    (** If [optional] is true then the whole [Set] query may match a list even if the
        optional term doesn't match any elements of the list. But if the term does match
        some element then there is no trivial match where the term does not capture
        anything. *)
    ; first_only : bool
    (** If [first_only] is true then the term only returns matches for the first element
        of the list that it successfully matches in at least one way. *)
    }
  [@@deriving sexp]
end

type t =
  | Capture_unlabeled of t
  (** [Capture_unlabeled t] is equivalent to [Capture_to_number (i, t)] where [i] starts
      at 0 and increments each time an unlabeled capture appears in the query. Syntax:
      [%.] *)
  | Capture_to_number of int * t
  (** [Capture_to_number (i, t)] applies the subquery [t], associating each consumed
      sequence to the label [Int.to_string i]. Example syntax: [%0] *)
  | Capture_to_name of string * t
  (** [Capture_to_name (n, t)] applies the subquery [t], associating each consumed
      sequence to the label [n]. Example syntax: [%foo] *)
  | Any (** Consume a single sexp or fail to match. Syntax: [.] *)
  | Atom of string
  (** Consumes an atom if it matches this exact string. Example syntax: [foo] *)
  | Atom_regex of string
  (** Consumes an atom if it matches this regex. Example syntax: [/foo/] *)
  | Sequence of t list
  (** Recursively applies each [t] against the remaining tail unconsumed by previous [t]s
      in the sequence. Example syntax: [[t1 t2 t3]] *)
  | Star of t
  (** Iteratively applies all of [[ Sequence []; Sequence [t]; Sequence [t; t]; ... ]],
      with a special-case guard against an infinite loop if [t] itself can consume zero
      elements. Example syntax: [t*] *)
  | Star_greedy of t
  (** Same as [Star], but tries possiblities in reverse order. Example syntax: [t*+] *)
  | Plus of t
  (** Iteratively applies all of [[ Sequence [t]; Sequence [t; t]; ... ]], with a
      special-case guard against an infinite loop if [t] itself can consume zero elements.
      Example syntax: [t+] *)
  | Plus_greedy of t
  (** Same as [Plus], but tries possiblities in reverse order. Example syntax: [t++] *)
  | Maybe of t
  (** Iteratively applies both of [[ Sequence []; Sequence [t] ]], with a special-case
      guard to match with multiplicity only 1 if [t] itself can consume zero elements.
      Example syntax: [t?] *)
  | Maybe_greedy of t
  (** Same as [Maybe], but tries possiblities in reverse order. Example syntax: [t?+] *)
  | List of t
  (** Consumes a sexp if it is a list, applying [t] to the sequence of the list's elements
      and requiring any successful matches by [t] to consume the entire sequence. Example
      syntax: [(t1 t2 t3)] *)
  | Set of (t * Set_kind.t) list
  (** Consumes a sexp if it is a list, recursively applying each subquery [t] iteratively
      one by one to each element of the sublist where that element is given as a singleton
      sequence, requiring that subquery to match that element. Multiple terms of the query
      may match the same list element. The cartesian product of all possible matches over
      the subqueries will be the result. See also the documentation above for [Set_kind]
      for how it may modify this behavior. Example syntax: [{t1 t2??}] *)
  | Subsearch of t
  (** [Subsearch t] matches some sequence {e S} of sexps if {e S} contains a subsequence
      which [t] matches, or if [List (Subsearch t)] matches one of the sexps in {e S}.
      Example syntax: [.. t] *)
  | And of t list
  (** Consumes a sequence of sexps only if every [t] provided has a successful match
      consuming exactly that sequence of sexps. Example syntax: [t1 & t2] *)
  | Or_shortcircuiting of t list
  (** Applies each [t] iteratively, stopping after iterating through all matches of the
      first [t] that has any matches. In other words,
      [Or_shortcircuiting [t1; t2; ...; ti; ...]] matches if and only if [ti] matches and
      none of queries before [ti] match. Example syntax: none *)
  | Or_all of t list
  (** Applies each [t] iteratively, matching according to the union of their matches. In
      other words, [Or_all [t1; ...; tn]] matches if and only if at least one of the [ti]
      matches. If more than one of the [ti] matches, it is undefined which of the matching
      terms will give the captures. Example syntax: [t1 | t2] *)
  | First_match_only of t
  (** Applies [t] and stops after the first time it matches. Example syntax: [!t] *)
[@@deriving sexp]

val iter : t -> f:(t -> unit) -> unit

module Capture_count : sig
  type t =
    { num_number_captures : int
    ; num_named_captures : int
    ; num_unlabeled_captures : int
    }
end

val count_captures : t -> Capture_count.t
val validate_all_captures_labeled_or_all_unlabeled_exn : t -> unit

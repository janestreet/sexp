open Core

(** Match a query against a sexp.

    If [wrap_singletons] is false, then as a convenience hack, within a given match, any
    time a capture consumes exactly one sexp (probably by far the common case in
    practical queries), it report that bare sexp as the capture, but if it captures zero
    or it captures two or more, then it will report it as a list (...).

    For example:
    "(%[.*])" tries to unwrap exactly one set of parens and capture all the sexps inside:

    () -> ()
    (a) -> a
    (a b) -> (a b)

    If instead [wrap_singletons] is true, we get this behavior, which is more
    machine-friendly and never ambiguous, but may put an extra undesired layer of wrapping
    around the user's desired output in the vast majority of practical cases:

    () -> ()
    (a) -> (a)
    (a b) -> (a b)

    If [output] is provided, substitutes all captures into the provided [output]
    structure. Otherwise, formats them heuristcally based on whether the captures were
    labeled or numbered and how many there were.
*)
val iter_matches
  :  query:Query.t
  -> output:Output.t option
  -> wrap_singletons:bool
  -> Sexp.t
  -> f:(Sexp.t -> unit)
  -> unit

(** Match a query against a sexp.
    For each successful match, perform a replacement where the subsexp corresponding
    to the [replace] label in that match gets replaced with the result of substituting
    all of the captures into [with_].

    If a match happens with replacement at both a sexp and one of its subsexps, the
    replacement only occurs for the outer sexp.
*)
val replace
  :  query:Query.t
  -> replace:string
  -> with_:Output.t
  -> wrap_singletons:bool
  -> Sexp.t
  -> Sexp.t

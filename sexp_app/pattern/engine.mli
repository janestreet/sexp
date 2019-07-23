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
*)
val iter_matches
  :  query:Query.t
  -> output_method:'output_type Output_method.Desired.t
  -> wrap_singletons:bool
  -> Sexp.t
  -> f:('output_type -> unit)
  -> unit

(** Match a query against a sexp. Then, for each successful match, perform a replacement
    where the subsexp or subsequence of sexps in the match corresponding to the [replace]
    label gets replaced according to [with_].

    The replacement is the result of substituting all of the captures of that match into
    the formats specified by [with_]. In most common use cases, [with_] will have length
    1, but it is also okay for it to be length 0 (delete the labeled sexp) or of greater
    length (replace the labeled sexp(s) with multiple sexps).

    If replacements would happen at both a sexp and one of its subsexps, the replacement
    only occurs for the outer sexp. If two replacements would overlap at the same level
    (e.g. in "a b c" one replacement removes "a b" and another removes "b c"), an
    arbitrary one of them will happen and the other will not.

    The return type is a list because if the *entire* sexp itself is the target being
    replaced, then the result of replacement will have length equal to [with_] upon
    any successful match, for example returning an empty list if [with_] specifies to
    delete the whole sexp upon a match. *)
val replace
  :  query:Query.t
  -> replace:string
  -> with_:Output_method.Format.t list
  -> wrap_singletons:bool
  -> Sexp.t
  -> Sexp.t list

(** [replace'] is like [replace] except that instead of a simple replacement, one can
    specify a function to compute replacements.

    [f] is passed a map from capture label to the sexp sequence captured by that label.
    [f] should return a map from capture label to the new sexp sequence that should be
    used as a replacement.

    If no replacement is desired for a given label, [f] can omit that label entirely
    in the map it returns - that part of the original sexp will then remain unchanged.

    If there are multiple matches and two different calls to [f] overlap in what subsexps
    they indicate should be replaced in the original underlying sexp, only the outermost
    and/or the first of those [f]'s replacements happen.

    Note that this means that there is a difference between [f] acting as the identity
    on a given label and [f] omitting returning that label entirely. In the first case,
    the identity replacement will happen, excluding any overlapping replacements, while
    the second will not exclude overlapping replacements.
*)
val replace'
  :  query:Query.t
  -> f:(Sexp.t list String.Map.t -> Sexp.t list String.Map.t)
  -> Sexp.t
  -> Sexp.t list

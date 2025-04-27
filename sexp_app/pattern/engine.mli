open Core

(** {v
 Match a query against a sexp.

    Calls [f] once for every match found, passing the captured results in the format
    specified by [output_method] as an argument.

    [wrap_mode] controls what happens when a capture consumes multiple sexps during a
    single match. It specifies whether to wrap them together as a single sexp list or
    return all the results separately.

    For example:
    "(a %[.*])" tries to unwrap exactly one set of parens, match an 'a', and then capture
    all the sexps that follow that 'a'. Here are the three behaviors.

    `Unwrap_always:
    (a) -> []                                              (*  *)
    (a b) -> [ Sexp.Atom b ]                               (* b *)
    (a b c) -> [ Sexp.Atom b; Sexp.Atom c ]                (* b c *)

    `Wrap_non_singletons:
    (a) -> [Sexp.List []]                                  (* () *)
    (a b) -> [ Sexp.Atom b ]                               (* b *)
    (a b c) -> [ Sexp.List [ Sexp.Atom b; Sexp.Atom c ] ]  (* (b c) *)

    `Wrap_always:
    (a) -> [Sexp.List []]                                  (* () *)
    (a b) -> [ Sexp.List [ Sexp.Atom b ] ]                 (* (b) *)
    (a b c) -> [ Sexp.List [ Sexp.Atom b; Sexp.Atom c ] ]  (* (b c) *)

    This wrapping (or not) occurs before packing the result into whatever format specified
    by [output_method].
    v} *)
val iter_matches
  :  query:Query.t
  -> output_method:'output_type Output_method.t
  -> Sexp.t
  -> f:('output_type -> unit)
  -> unit

(** Match a query against a sexp. Then, for each successful match, perform a replacement
    where the subsexp or subsequence of sexps in the match corresponding to the [replace]
    label gets replaced according to [with_] and [wrap_mode].

    The replacement is the result of substituting all of the captures of that match into
    the formats specified by [with_]. I.e. it replaces the sexp sequences of each capture
    of [replace] with what would be output by [Output_method.Formats (wrap_mode, with_)].
    In most common use cases, [with_] will have length 1, but it is also okay for it to be
    length 0 (delete the labeled sexp) or of greater length (replace the labeled sexp(s)
    with multiple sexps).

    If replacements would happen at both a sexp and one of its subsexps, the replacement
    only occurs for the outer sexp. If two replacements would overlap at the same level
    (e.g. in "a b c" one replacement removes "a b" and another removes "b c"), an
    arbitrary one of them will happen and the other will not.

    The return type is a list because if the *entire* sexp itself is the target being
    replaced, then the result of replacement will have length equal to [with_] upon any
    successful match, for example returning an empty list if [with_] specifies to delete
    the whole sexp upon a match. *)
val replace
  :  query:Query.t
  -> replace:string
  -> with_:Output_method.Format.t list
  -> wrap_mode:_ Output_method.Wrap_mode.t
  -> Sexp.t
  -> Sexp.t list

(** [replace'] is like [replace] except that instead of a simple replacement, one can
    specify a function to compute replacements.

    [f] is passed a map from capture label to the sexp sequence captured by that label.
    [f] should return a map from capture label to the new sexp sequence that should be
    used as a replacement.

    If no replacement is desired for a given label, [f] can omit that label entirely in
    the map it returns - that part of the original sexp will then remain unchanged.

    If there are multiple matches and two different calls to [f] overlap in what subsexps
    they indicate should be replaced in the original underlying sexp, only the outermost
    and/or the first of those [f]'s replacements happen.

    Note that this means that there is a difference between [f] acting as the identity on
    a given label and [f] omitting returning that label entirely. In the first case, the
    identity replacement will happen, excluding any overlapping replacements, while the
    second will not exclude overlapping replacements. *)
val replace'
  :  query:Query.t
  -> f:(Sexp.t list String.Map.t -> Sexp.t list String.Map.t)
  -> Sexp.t
  -> Sexp.t list

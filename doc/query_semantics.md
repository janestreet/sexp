# Sexp query formal semantics

See also the get and select and pat-query subcommands.

This document explains the semantics of query expressions, whose
syntax is as follows:

    E ::=
        -- selection -------------
        | (index NUM)
        | (field STRING)
        | each
        | smash
        -- composition -----------
        | (pipe E E ...)
        | (cat E E ...)
        | this
        | none
        -- tests/conditionals ----
        | atomic
        | (variant TAG NUM)
        | (equals SEXP ...)
        | (regex R)
        | (test E E ...)
        | (not E)
        | (and E E ...)
        | (or E E ...)
        | (if E E E)
        | (branch E E E)
        -- formatting ------------
        | (wrap E)
        | (quote T[0])
        -- transformations -------
        | (change C)
        | restructure
        | length

The meaning of an expression is a function from an input s-expression to a
(possibly empty) sequence of output s-expressions.  The toplevel command is
applied to each expression in the input.  We will look at five categories
of expressions: selection, composition, conditionals, formatting, and
transformation.

Selection
---------

An `(index N)` expression picks out the Nth element of a list s-expression, or the
Nth-from-last element if N is negative. It succeeds if N is a valid index (i.e., -(list
length) <= N < list length) and fails otherwise. Upon success, it returns the
single-element sequence containing the selected sub-expressions. Upon failure, it returns
an empty list.

    (index 2)  : (one two three four) => {three}
    (index 8)  : (one two three four) => {}
    (index -1) : (one two three four) => {four}
    (index -5) : (one two three four) => {}

A `(field F)` expression is like `(index N)` except that it projects
the field named F out of a record.  If more than one field of that
name exists, they will all be selected.

    (field foo) : ((bar 1) (foo 2) (baz 3))         => {2}
    (field foo) : ((bar 1) (foo 2) (baz 3) (foo 4)) => {2, 4}
    (field wow) : ((bar 1) (foo 2) (baz 3))         => {}

An `each` expression selects every element of a list.

    each : (one two three four) => {one, two, three, four}
    each : ()    => {}
    each : hello => {}

A `smash` expression selects every sub-expression of an s-expression

    smash : (a (b c) (d (e f))) => { (a (b c) (d (e f))),
                                      a, (b c), (d (e f)),
                                          b, c,  d, (e f),
                                                     e, f }

Composition
-----------

The two basic composition operators are `pipe` and `cat`.  These two
operators do sequential and parallel composition, respectively.  The
denotation of

                    (pipe E1 E2 ... En)

is a "fanned out" composition of the denotations D = [E1] and
D' = [`(pipe E2 ... En)`] in which D' is called on every s-expression
in the output sequence of D and all the resulting sequences of
s-expressions are concatenated together into one big sequence.

            (pipe E) = E
            (pipe E1 E2 ... En) : S => X1 U ... U Xm

            where   E1 : S => {S1, ..., Sm}
              and   (pipe E2 ... En) : Si => Xi   for i in {1..m}

In particular, this means that if D returns an empty sequence, then
D' will never be called.  The denotation of

                    (cat E1 E2 ... En)

is simply a parallel execution of each Ei on the input X in which all

            (cat E1 ... En) : S => X1 U ... U Xn

            where   Ei : S => Xi    for i in {i .. n}

There are two trivial expressions, `this` and `none`.  A this expression
simply selects it input, while a `none` expression selects nothing

                this : S => {S}         none : S => {}

These are identities for the pipe and cat operators.

                this = (pipe)          none = (cat)

(For monad fans -- pipe is Kleisli composition and `this` is Kleisli
identity for the list monad.)


Conditionals
------------

For the purpose of conditional execution, we treat an expression returning
a non-empty sequence as true, and an expression returning an empty sequence
as false.  With this in mind, one may think of `this` and `none` as the
constants true and false, respectively.

A `atomic` expression tests for atomic s-expressions.

        atomic : foo       => {foo}
        atomic : (foo bar) => {}

The most basic non-trivial condition is equality.  The expression
`(equals S)` selects its input in the case that the (returns true) and
fails (returns false) otherwise.

        (equals S) : S' => {S'}    if S = S'
        (equals S) : S' => {}      otherwise

There is also a N-ary version of equals that is expands to a disjunction

        (equals S1 S2 ...) = (or (equals S1) (equals S2) ...)

A `(regex R)` expression tests for the s-expresion to be an ATOM that
matches R.  The truth value will be the first capturing group of the
regular expression or the whole atom if regular expression has no
capturing groups.

A `(variant F N)` expression tests for the s-expression form of an element
of a variant type with constructor F applied to N arguments.  You can leave N off, in
which case it will match a variant with any number of arguments.

    (variant foo 5) : (foo 1 2 3 4 5) => {(foo 1 2 3 4 5)}
    (variant foo 3) : (foo 1 2 3 4 5) => {}
    (variant foo 8) : (foo 1 2 3 4 5) => {}
    (variant bar 5) : (foo 1 2 3 4 5) => {}
    (variant foo 0) : foo => {foo}
    (variant foo 0) : (foo) => {(foo)}
    (variant foo)   : (foo 1 2 3 4 5) => {(foo 1 2 3 4 5)}
    (variant foo)   : foo => {foo}
    (variant foo)   : (foo) => {(foo)}

A `(test E)` expression proceeds by evaluating E on the current s-expression
S and selecting S only in the case that E succeeds.

        (test E) : S => {}    if X empty      where  E : S => X
        (test E) : S => {S}   otherwise

We also provide `(test E1 E2 ...)` as syntactic sugar for the common
idiom `(test (pipe E1 E2 ...))`.

A `(not E)` expression proceeds by evaluating E on the current s-expression
S and selecting S only in the case that E fails.

        (not E) : S => {S}    if X empty      where  E : S => X
        (not E) : S => {}     otherwise

Note that X is is discarded from the output of both (test E) and (not E).
For this reason, these two operators are useful for "looking down" into an
s-expression while remembering your place at some point above where you will
return later.

An `(and E1 ... En)` expression does short-circuit evaluation based on whether
or not E1 succeeds.  Upon success, it returns the results of En.

    (and) = this

    (and E) = E

    (and E1 E2 ... En) : S => {}        if  E1 : S => {}
    (and E1 E2 ... En) : S => Y         if  E1 : S => X  (nonempty)
                                        and (and E2 ... En) : S => Y

An `(or E1 ... En)` expression does short-circuit evaluation based on whether
or not E1 succeeds.  It returns the results of the first Ei that succeeds.

    (or) = none

    (or E) = E

    (or E1 E2 ... En) : S => X         if  E1 : S => X  (nonempty)
    (or E1 E2 ... En) : S => Y         if  E1 : S => {}
                                       and (or E2 ... En) : S => Y

An `(if E1 E2 E3)` expression does conditional execution of E2 or E3 based
on whether or not E1 succeeds.

    (if E1 E2 E3) : S => X2      if E1 : S => X (non-empty) and
    (if E1 E2 E3) : S => X3      if E1 : S => {}

       where   Ei : Si => Xi    for i in {2,3}

An `(branch E1 E2 E3)` expression does conditional execution like `if`, but
also pipes the output of the condition into the `then' branch.

    (branch E1 E2 E3) : S => X1 U ... U Xn

                                    if E1 : S => {S1, ... , Sn} (non-empty)
                                    and E2 : Si => Xi  for i in {1..n}

    (branch E1 E2 E3) : S => X3
                                    if E1 : S => {}
                                    and E3 : S3 => X3

The following equations relating the behavior of `if` and `branch` are true:

    (if E1 E2 E3) = (branch (test E1) E2 E3)

    (branch E1 E2 E3) = (if E1 (pipe E1 E2) E3)


Formatting
----------

Using the commands so far, one may only output sub-expressions of the input
expression itself.  However, we may also want to impose additional structure
on the output.

A `(wrap E)` expression runs E and gathers up the resulting sequence into a
single list s-expression that becomes the (single) overall result.

    (wrap E) : S => {(S1 ... S2)}  where  E : S => {S1, ... , S2}

Note that the final output sequence has exactly one element that is a list.

A `(quote S)` expression adds the provided s-expression to the manifest.

    (quote S') : S => {S'}

Note that S is discarded here.  For this reason, quote is often used in
conjunction with a parallel composition operator like `cat`, `and`, or `or`.

    (quote (a b c))              : (1 2 3) => {(a b c)}
    (quote (a (unquote each) c)) : (1 2 3) => {(a 1 c), (a 2 c), (a 3 c)}
    (quote (a (splice each) c))  : (1 2 3) => {(a 1 2 3 c)}

    (quote (a (splice each) c (unquote each)))
        : (1 2 3) => {(a 1 2 3 c 1), (a 1 2 3 c 2), (a 1 2 3 c 3)}

Multiple unquotes in a single quoted template yield a cartesian product.

    (quote (a (unquote (pipe (index 0) each))
            b (unquote (pipe (index 1) each))))
        : ((1 2 3) (x y z)) => {(a 1 b x), (a 1 b y), (a 1 b z),
                                (a 2 b x), (a 2 b y), (a 2 b z),
                                (a 3 b x), (a 3 b y), (a 3 b z)}

Furthermore, nested quotes increase the "degree" of quotation.  Splice and
unquote only have their effect at the degree zero.  This feature is intended
to facilitate sexpquery expressions that manipulate other sexpquery expressions.

--- grammar for sexpquery templates ---

          T[0] ::= ATOM               T[n+1] ::= ATOM
                 | (T[0] ... T[0])             | (T[n+1] ... T[n+1])
                 | (quote T[1])                | (quote T[n+2])
                 | (unquote E)                 | (unquote T[n])
                 | (splice E)                  | (splice T[n])

Transformations
---------------

Finally, we have a way to call the change language from the query
language.  The semantics of (change C) are to return the transformed
expression as a singleton sequence and propagate failure.

    (change C) : S => {S'}    if C : S => S'
    (change C) : S => {}      if C : S => _|_

(see the change subcommand's internal documentation for information on
change semantics).

Sometimes the contents of an atom will be the string representation of
a sequence of sexps. `restructure` will do this interpretation for you:

    restructure : "A (B C) D" => {A (B C) D}

Sometimes you want the number of elements in a list.
    
    length : ATOM => {1}
    length : (T[1] ... T[N]) => {N}

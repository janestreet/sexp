# Sexp change formal semantics

This document explains the semantics of change expressions, whose
syntax is as follows:

     C ::= (rewrite P P)
         | (seq C C ...)
         | (alt C C ...)
         | id
         | fail
         | delete
         | (const SEXP)
         | (try C)
         | (record (F C) ...)
         | (children C)
         | (topdown C)
         | (bottomup C)
         | lowercase
         | concat
         | (query Q)

    F ::= ATOM

The meaning of an expression is a partial function on s-expressions.
The toplevel command is applied to each input expression in the input.
(NOTE: this fragment of the language is essentially a fragment of Stratego
term rewriting language: http://strategoxt.org)

Rewriting
=========

A `(rewrite LHS RHS)` attempts to rewrite an expression by the specified
rewrite rule.  It first matches the input s-expression against the
LHS pattern.  If this succeeds, it instantiates the RHS pattern with
bindings inferred from the match.

The syntax of the LHS and RHS patterns is as follows:

     P ::= ATOM | (P ... P) | $VAR | @VAR

Note that variables are distinguished from atoms by a prefix of `$` or `@`.
Variables starting with `$` are scalar and they match a single s-expression,
while those starting with `@` match a list of s-expressions.
To match literal atoms starting with `$` or `@`, repeat the prefix character.

Some examples

                (rewrite foo bar) : foo => bar
                (rewrite foo bar) : abc => _|_          (indicates failure)
                (rewrite foo bar) : (foo bar) => _|_
          (rewrite (foo bar) wow) : (foo bar) => wow
            (rewrite (foo $X) $X) : (foo bar) => bar
            (rewrite (foo $X) $X) : (foo (bar none)) => (bar none)
       (rewrite (foo $X) ($X $X)) : (foo bar) => (bar bar)
          (rewrite (foo @X) (@X)) : (foo bar baz) => (bar baz)
          (rewrite (foo @X) (@X)) : (foo (bar a) (baz b)) => ((bar a) (baz b))
       (rewrite (foo @X) (@X @X)) : (foo bar baz) => (bar baz bar baz)
    (rewrite ($$foo $X) (bar $X)) : ($foo wow) => (bar wow)
    (rewrite (foo $X) (@@bar $X)) : (foo wow) => (@bar wow)

Some rewrite rules are not well formed.  The following rules apply:

    well-formedness rule                          non-conforming program
    -----------------------------------------------------------------------
    variables may not be bound more than once     (rewrite (foo $X $X) who)
    all RHS variables must be bound on LHS        (rewrite (foo bar) (yo $X))
    no two list variables can be in the same list (rewrite (foo @X @Y) @X)

`(const SEXP)` is syntactic sugar for `(rewrite $_ SEXP)`.  That is, const is a
rewrite rule that always succeeds and produces the same sexp.

`(rewrite_record LHS RHS)` should be used if order of s-expressions in lists
should be disregarded.  Some examples

    (rewrite_record (foo bar) wow) : (bar foo) => wow
    (rewrite_record (foo bar) wow) : (foo bar) => wow
    (rewrite_record (foo bar) wow) : (foo) => _|_
    (rewrite_record (foo bar) wow) : (bar) => _|_
    (rewrite_record (bar @X) (wow @X)) : (foo bar baz) => (wow foo baz)

Composition
===========

The two basic composition operators are `seq` and `alt`.  These two
operators provide sequencing and biased choice, respectively.  The
denotation of

                    (seq C1 C2 ... Cn)

is a composition of the denotations D = [C1] and D' = [(seq C2 ... Cn)].

    (seq C1 C2 ... Cn) : S => S''    if C1 : S => S'
                                   and (seq C2 ... Cn) : S' => S''

Failure at any point in the sequence propagates out to the whole.

    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => _|_

    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => S'
                                   and (seq C2 ... Cn) : S' => _|_

Unary application of seq has no effect

    (seq C) = C

The denotation of

                    (alt C1 C2 ... Cn)

is simply a biased choice of whichever of C1 ... Cn first succeeds on
the input.

    (alt C1 C2 ... Cn) : S => S'   if C1 : S => S'

    (alt C1 C2 ... Cn) : S => R    if C1 : S => _|_ and (alt C2 ... Cn) : S => R
                                 (where R is the either _|_ or an s-expression)

There are two trivial expressions, `id` and `fail`.  A id expression
simply selects it input, while a `fail` expression selects nothing

                id : S => S           fail : S => _|_

These are identities for the seq and alt operators.

                id = (seq)            fail = (alt)

One other pattern is so common that it is part of the language itself.

                (try C) = (alt C id)

Records
=======

The `record` syntax is used to rewrite record sexps of the form
((field1 value1) ... (fieldn value_n)).  With `record`, one specifies for
each field name how to change the value of the field with that name, and
optionally what to do with fields not explicitly mentioned.  For example:

    (record (a1 delete) (a2 (const 13)) (a3 (rewrite $X ($X $X))))
    :  ((a1 v1) (a2 v2) (a3 v3))
    => (        (a2 13) (a3 (v3 v3)))

By default, a record rewrite only succeeds if all of the fields appear in the
input.  Also by default, fields that appear in the input that are not mentioned
in the record are preserved.

One can allow the rewrite to succeed even if a field is missing by adding an
attribute.

    (record (f1            delete)) : ((f2 v2)) => _|_
    (record (f1 (optional) delete)) : ((f2 v2)) => ((f2 v2))

Fields that are in the record but not in the input are treated as if their
value is ().  This makes it possible to add fields.  For example:

    (record (a1 (optional) id         )) : () -> ((a1 ()))
    (record (a1 (optional) (const foo))) : () -> ((a1 foo))

One can use the special `_` as a field name to explicitly specify what to
do with fields in the input that aren't mentioned in the record.

    (record (a1 (const 13)) (_ id))    # the default, same as not using _
    :   ((a1 v1) (a2 v2))
    ==> ((a1 13) (a2 v2))

    (record (a1 id) (_ delete))        # delete unmentioned fields
    :   ((a1 v1) (a2 v2) (a3 v3))
    ==> ((a1 v1))

    (record (a1 id) (_ fail))         # fail if there are unmentioned fields
    :   ((a1 v1) (a2 v2)
    ==> _|_

One can optionally specify a new field name, which will cause the field
name to be changed.

    (record (a1 ((rename a2)) id)) : ((a1 13)) -> ((a2 13))

There are a couple of well-formedness rules on record syntax.

    well-formedness rule                          non-conforming program
    -----------------------------------------------------------------------------
    a field name can occur at most once           (record (foo id) (foo delete))
    if there an _ field, it must occur last       (record (_ delete) (foo id))

Traversals
==========

The expression `(children C)` transforms all the immediate
sub-expressions of a s-expression according to C and returns the result.

          (children (rewrite foo bar)) : (foo foo) => (bar bar)

If C fails on any of the children, the failure propagates outward.

          (children (rewrite foo bar)) : (foo wow) => _|_

This is easily overcome, if desired

    (children (try (rewrite foo bar))) : (foo wow) => (bar wow)

If the input is atomic (i.e., there are no children), then C is trivially
successful on *all* the children.

          (children (rewrite foo bar)) : wow => wow

Top-down and bottom-up iterated traversal strategies are defined recursively
in terms of children expressions.

    (topdown C) = (seq C (children (topdown C)))

    (bottomup C) = (seq (children (bottomup C)) C)

For example:

     (topdown (try (rewrite a b))) : (a (c a)) => (b (c b))
    (bottomup (try (rewrite a b))) : (a (c a)) => (b (c b))

The difference between topdown and bottomup can be seen here:

    if

    C = (try (rewrite (not (and $A $B)) (or (not $A) (not $B))))

    then

    (topdown C)
      : (not (and a (and b c))) => (or (not a) (or (not b) (not c)))

    but

    (bottomup C)
      : (not (and a (and b c))) => (or (not a) (not (and b c)))

Note that one must take care when using topdown to avoid infinite loops!

    (topdown (rewrite a (a a))) : a => ...      (never returns!)

Deletion
========

`delete` allows one to delete a sexp from its containing sexp.  Semantically,
it is essentially a special kind of return value that is recognized by other
constructs, like `children`, and causes them to delete the component sexp.

    delete : foo => _|_
     (children delete) : (foo bar) => ()
     (children (alt (rewrite foo 13) delete)) : (foo bar) => (13)

`delete` satisfies the following equalities

    (seq delete C) == delete
    (alt delete C) == delete

Miscellaneous
=============

`lowercase` does exactly what you would think it does.

    lowercase : Word        => word
    lowercase : UPPERCASE   => uppercase
    lowercase : CamelCase   => camelcase
    lowercase : (A (B C) D) => (a (b c) d)
    lowercase : 1234        => 1234

`concat` concatenates all the atoms of the input expression into a single
atom.

    concat : Word          => Word
    concat : (' "A B" ') => "'A B'"
    concat : (A (B C) D)   => ABCD

Sub-queries
===========

Finally, we have a way to call the query language from the change
language.  The semantics of `(query Q)` are to gather up all the
s-expressions output by running the query Q against the input and
gathering them all up into a single list.

    (query Q) : S => (X1 ... Xn)    if Q : S => {X1, ... , Xn}

(see the query subcommand's internal documentation for information on
query semantics).

Note that this operation always succeeds (never results in _|_).

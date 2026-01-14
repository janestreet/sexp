let change_by_example_dot_md =
  "# Sexp change by example\n\n\
   Sexp change is a command-line tool for manipulating S-expressions,\n\
   useful if you want to do some data munging without having to write a\n\
   full-fledged program to parse and process your sexps.\n\n\
   This doc collects examples of sexp change in action. For a more\n\
   complete overview of the language, see\n\
   [this doc](./change_semantics.md),\n\
   which includes many small examples showing how to use each keyword.\n\n\
   Applying a rewrite rule throughout a file\n\
   -----------------------------------------\n\n\
   Perhaps the most common way to use `sexp change` is just to barrel\n\
   through a file attempting to apply a rewrite rule. You can accomplish\n\
   this with the following:\n\n\
   ```\n\
   (topdown (try (rewrite LHS RHS)))\n\
   ```\n\n\
   The `topdown` keyword ensures you cover the entire file, while `try`\n\
   ensures that if you can't apply the rewrite rule to a given expression\n\
   LHS, you leave LHS as it is.\n\n\
   For convenience, this functionality is exposed with the `sexp rewrite` command.\n\n\
   Using sexp query and sexp change together\n\
   -----------------------------------------\n\n\
   Here are a pair of examples making use of sexp query's `index`,\n\
   `pipe`, `quote` and `unquote` commands, and sexp change's `seq`,\n\
   `rewrite`, and `concat` commands.\n\n\
   Suppose your input is something like:\n\n\
   ```\n\
   (AMZN ( ... ))\n\
   (MSFT ( ... ))\n\
   ```\n\n\
   and you want to change it to:\n\n\
   ```\n\
   (\"AMZN US\" ( ... ))\n\
   (\"MSFT US\" ( ... ))\n\
   ```\n\n\
   Here's how you could do it using the `change` subcommand of `sexp query`:\n\n\
   ```sh\n\
   sexp query '\n\
   (quote (\n\
  \  (unquote (pipe (index 0) (change (seq (rewrite $X ($X \" US\")) concat))))\n\
  \  (unquote (index 1))))\n\
   ' <<EOF\n\
   (AMZN (foo bar))\n\
   (MSFT (foo bar))\n\
   EOF\n\
   ```\n\n\
   Let's break down how this works. From\n\
   [the sexp query readme](query_by_example.md) you know how to use\n\
   `quote` to create an S-expression \"template\" that is then filled in\n\
   with `unquote` statements. Here, that template is going to look like\n\
   `(A B)`, where the `A` part is filled in by the results of:\n\n\
   ```sh\n\
   (pipe (index 0) (change (seq (rewrite $X ($X \" US\")) concat)))\n\
   ```\n\n\
   We pipe the results of `(index 0)`, i.e., something like \"AMZN\", to\n\
   the `change` command, which consists of a `seq`-uence of two\n\
   subcommands: `(rewrite $X ($X \" US\"))` takes the single S-expression\n\
   like `AMZN` (an atom) and rewrites it as `(AMZN \" US\")`. `concat` then\n\
   concatenates this 2-atom list into the single atom `\"AMZN US\"`.\n\n\
   Finally, the second half of the `quote` statement plops the result of\n\
   `(unquote (index 1))` into the latter half of the list, creating the\n\
   desired `(AMZN (foo bar))`.\n\n\
   Suppose we wanted to do this for sexps of more than two items, like\n\
   so:\n\n\
   ```\n\
   (AMZN ( ... ) ( ... ))\n\
   (MSFT ( ... ) ( ... ) ( ... ))\n\
   ```\n\n\
   Then you'd want to use the `query` subcommand of `sexp change`:\n\n\
   ```sh\n\
   sexp change '\n\
   (seq\n\
  \  (rewrite ($A @B) ($A (@B)))\n\
  \  (query (\n\
  \    quote (\n\
  \      (unquote (pipe (index 0) (change (seq (rewrite $X ($X \" US\")) concat))))\n\
  \      (unquote (index 1)))))\n\
  \  (rewrite (($A (@B))) ($A @B)))\n\
   ' <<EOF\n\
   (AMZN (foo bar) baz)\n\
   (MSFT (foo bar))\n\
   EOF\n\
   ```\n\n\
   Notice how the inner part of this command is the exact same `query`\n\
   we just did. But that's step 2 of a sequence of three steps, the first\n\
   of which is to wrap the \"tail\" of our list in parens, via:\n\n\
   ```sh\n\
   (rewrite ($A @B) ($A (@B)))\n\
   ```\n\n\
   and then, after we're done with the meat of the rewrite, unwrap it\n\
   again via the inverse:\n\n\
   ```sh\n\
   (rewrite (($A (@B))) ($A @B))\n\
   ```\n\n\
   where the extra parens around `(($A (@B)))` are added because the\n\
   `query` subcommand of `sexp change` wraps its results in a list.\n\n\
   Overwriting all record values with a given key\n\
   ----------------------------------------------\n\n\
   See the following little shell command:\n\n\
   ```sh\n\
   function proxy-iron-config-command {\n\
  \    no-args \"$@\"\n\
  \    host=james\n\
  \    sexp change </etc/iron-config.sexp \\\n\
  \         \"(record (host (const $host)))\"\n\
   }\n\
   ```\n\n\
   This operates on a file with S-expressions of the form:\n\n\
   ```ocaml\n\
   ((host some_host_name)\n\
  \ (async_rpc_port (Static 7013))\n\
  \ (hgrc /path/to/hgrc)\n\
  \ (serializer_pause_timeout 2m)\n\
  \ (rpc_proxy_config\n\
  \  ((another_host1 (p1 p2 p3))\n\
  \   (another_host2 (p1 p2)))))\n\
   ```\n\n\
   i.e., records with keys `host`, `async_rpc_port`, `hgrc`,\n\
   `serializer_pause_timeout`, and so on. By writing\n\
   `(record (host (const $host)))`, the above sexp change program just\n\
   takes whatever the value of the interpolated `$host` ends up being --\n\
   this is a shell variable, not a variable in the sexp change language\n\
   -- and jams it into the place of _every_ `host` record value. So if\n\
   `$host` were \"foo\", the above record would be rewritten as:\n\n\
   ```ocaml\n\
   ((host foo)\n\
  \ (async_rpc_port (Static 7013))\n\
  \ (hgrc /path/to/hgrc)\n\
  \ (serializer_pause_timeout 2m)\n\
  \ (rpc_proxy_config\n\
  \  ((another_host1 (p1 p2 p3))\n\
  \   (another_host2 (p1 p2)))))\n\
   ```\n\n\
   and so on for the rest of the records in the file -- they would all\n\
   begin `(host foo)`.\n\n\
   Doing a bottom-up search with alt and try\n\
   -----------------------------------------\n\n\
   Here, we use the `alt` keyword for the simple reason that it allows\n\
   us to apply several different rewrite rules in parallel, in a single\n\
   pass through a file:\n\n\
   ```\n\
   sexp change '(bottomup (try (alt\n\
  \    (rewrite (App $X $Y) ($X $Y))\n\
  \    (rewrite (Name $X) $X)\n\
  \    (rewrite (Var (Free $X)) $X)\n\
  \  )))'\n\
   ```\n\n\
   The `alt` keyword is short-circuiting, in the sense that it returns\n\
   as soon as one of the rewrite rules you pass it succeeds. Here, we don't\n\
   really take advantage of that short-circuiting: no matter what order\n\
   we put our three rules in, the program will have the same behavior.\n\n\
   If, however, the LHS of our alternative rewrite rules had the same\n\
   structure -- like if they all looked like `(Foo $X) RHS` or `(Foo $X)\n\
   RHS'` -- then there would at least be the chance that the order of\n\
   the rules could matter.\n"
;;

let change_semantics_dot_md =
  "# Sexp change formal semantics\n\n\
   This document explains the semantics of change expressions, whose\n\
   syntax is as follows:\n\n\
  \     C ::= (rewrite P P)\n\
  \         | (seq C C ...)\n\
  \         | (alt C C ...)\n\
  \         | id\n\
  \         | fail\n\
  \         | delete\n\
  \         | (const SEXP)\n\
  \         | (try C)\n\
  \         | (record (F C) ...)\n\
  \         | (children C)\n\
  \         | (topdown C)\n\
  \         | (bottomup C)\n\
  \         | lowercase\n\
  \         | concat\n\
  \         | (query Q)\n\n\
  \    F ::= ATOM\n\n\
   The meaning of an expression is a partial function on s-expressions.\n\
   The toplevel command is applied to each input expression in the input.\n\
   (NOTE: this fragment of the language is essentially a fragment of Stratego\n\
   term rewriting language: http://strategoxt.org)\n\n\
   Rewriting\n\
   =========\n\n\
   A `(rewrite LHS RHS)` attempts to rewrite an expression by the specified\n\
   rewrite rule.  It first matches the input s-expression against the\n\
   LHS pattern.  If this succeeds, it instantiates the RHS pattern with\n\
   bindings inferred from the match.\n\n\
   The syntax of the LHS and RHS patterns is as follows:\n\n\
  \     P ::= ATOM | (P ... P) | $VAR | @VAR\n\n\
   Note that variables are distinguished from atoms by a prefix of `$` or `@`.\n\
   Variables starting with `$` are scalar and they match a single s-expression,\n\
   while those starting with `@` match a list of s-expressions.\n\
   To match literal atoms starting with `$` or `@`, repeat the prefix character.\n\n\
   Some examples\n\n\
  \                (rewrite foo bar) : foo => bar\n\
  \                (rewrite foo bar) : abc => _|_          (indicates failure)\n\
  \                (rewrite foo bar) : (foo bar) => _|_\n\
  \          (rewrite (foo bar) wow) : (foo bar) => wow\n\
  \            (rewrite (foo $X) $X) : (foo bar) => bar\n\
  \            (rewrite (foo $X) $X) : (foo (bar none)) => (bar none)\n\
  \       (rewrite (foo $X) ($X $X)) : (foo bar) => (bar bar)\n\
  \          (rewrite (foo @X) (@X)) : (foo bar baz) => (bar baz)\n\
  \          (rewrite (foo @X) (@X)) : (foo (bar a) (baz b)) => ((bar a) (baz b))\n\
  \       (rewrite (foo @X) (@X @X)) : (foo bar baz) => (bar baz bar baz)\n\
  \    (rewrite ($$foo $X) (bar $X)) : ($foo wow) => (bar wow)\n\
  \    (rewrite (foo $X) (@@bar $X)) : (foo wow) => (@bar wow)\n\n\
   Some rewrite rules are not well formed.  The following rules apply:\n\n\
  \    well-formedness rule                          non-conforming program\n\
  \    -----------------------------------------------------------------------\n\
  \    variables may not be bound more than once     (rewrite (foo $X $X) who)\n\
  \    all RHS variables must be bound on LHS        (rewrite (foo bar) (yo $X))\n\
  \    no two list variables can be in the same list (rewrite (foo @X @Y) @X)\n\n\
   `(const SEXP)` is syntactic sugar for `(rewrite $_ SEXP)`.  That is, const is a\n\
   rewrite rule that always succeeds and produces the same sexp.\n\n\
   `(rewrite_record LHS RHS)` should be used if order of s-expressions in lists\n\
   should be disregarded.  Some examples\n\n\
  \    (rewrite_record (foo bar) wow) : (bar foo) => wow\n\
  \    (rewrite_record (foo bar) wow) : (foo bar) => wow\n\
  \    (rewrite_record (foo bar) wow) : (foo) => _|_\n\
  \    (rewrite_record (foo bar) wow) : (bar) => _|_\n\
  \    (rewrite_record (bar @X) (wow @X)) : (foo bar baz) => (wow foo baz)\n\n\
   Composition\n\
   ===========\n\n\
   The two basic composition operators are `seq` and `alt`.  These two\n\
   operators provide sequencing and biased choice, respectively.  The\n\
   denotation of\n\n\
  \                    (seq C1 C2 ... Cn)\n\n\
   is a composition of the denotations D = [C1] and D' = [(seq C2 ... Cn)].\n\n\
  \    (seq C1 C2 ... Cn) : S => S''    if C1 : S => S'\n\
  \                                   and (seq C2 ... Cn) : S' => S''\n\n\
   Failure at any point in the sequence propagates out to the whole.\n\n\
  \    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => _|_\n\n\
  \    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => S'\n\
  \                                   and (seq C2 ... Cn) : S' => _|_\n\n\
   Unary application of seq has no effect\n\n\
  \    (seq C) = C\n\n\
   The denotation of\n\n\
  \                    (alt C1 C2 ... Cn)\n\n\
   is simply a biased choice of whichever of C1 ... Cn first succeeds on\n\
   the input.\n\n\
  \    (alt C1 C2 ... Cn) : S => S'   if C1 : S => S'\n\n\
  \    (alt C1 C2 ... Cn) : S => R    if C1 : S => _|_ and (alt C2 ... Cn) : S => R\n\
  \                                 (where R is the either _|_ or an s-expression)\n\n\
   There are two trivial expressions, `id` and `fail`.  A id expression\n\
   simply selects it input, while a `fail` expression selects nothing\n\n\
  \                id : S => S           fail : S => _|_\n\n\
   These are identities for the seq and alt operators.\n\n\
  \                id = (seq)            fail = (alt)\n\n\
   One other pattern is so common that it is part of the language itself.\n\n\
  \                (try C) = (alt C id)\n\n\
   Records\n\
   =======\n\n\
   The `record` syntax is used to rewrite record sexps of the form\n\
   ((field1 value1) ... (fieldn value_n)).  With `record`, one specifies for\n\
   each field name how to change the value of the field with that name, and\n\
   optionally what to do with fields not explicitly mentioned.  For example:\n\n\
  \    (record (a1 delete) (a2 (const 13)) (a3 (rewrite $X ($X $X))))\n\
  \    :  ((a1 v1) (a2 v2) (a3 v3))\n\
  \    => (        (a2 13) (a3 (v3 v3)))\n\n\
   By default, a record rewrite only succeeds if all of the fields appear in the\n\
   input.  Also by default, fields that appear in the input that are not mentioned\n\
   in the record are preserved.\n\n\
   One can allow the rewrite to succeed even if a field is missing by adding an\n\
   attribute.\n\n\
  \    (record (f1            delete)) : ((f2 v2)) => _|_\n\
  \    (record (f1 (optional) delete)) : ((f2 v2)) => ((f2 v2))\n\n\
   Fields that are in the record but not in the input are treated as if their\n\
   value is ().  This makes it possible to add fields.  For example:\n\n\
  \    (record (a1 (optional) id         )) : () -> ((a1 ()))\n\
  \    (record (a1 (optional) (const foo))) : () -> ((a1 foo))\n\n\
   One can use the special `_` as a field name to explicitly specify what to\n\
   do with fields in the input that aren't mentioned in the record.\n\n\
  \    (record (a1 (const 13)) (_ id))    # the default, same as not using _\n\
  \    :   ((a1 v1) (a2 v2))\n\
  \    ==> ((a1 13) (a2 v2))\n\n\
  \    (record (a1 id) (_ delete))        # delete unmentioned fields\n\
  \    :   ((a1 v1) (a2 v2) (a3 v3))\n\
  \    ==> ((a1 v1))\n\n\
  \    (record (a1 id) (_ fail))         # fail if there are unmentioned fields\n\
  \    :   ((a1 v1) (a2 v2)\n\
  \    ==> _|_\n\n\
   One can optionally specify a new field name, which will cause the field\n\
   name to be changed.\n\n\
  \    (record (a1 ((rename a2)) id)) : ((a1 13)) -> ((a2 13))\n\n\
   There are a couple of well-formedness rules on record syntax.\n\n\
  \    well-formedness rule                          non-conforming program\n\
  \    -----------------------------------------------------------------------------\n\
  \    a field name can occur at most once           (record (foo id) (foo delete))\n\
  \    if there an _ field, it must occur last       (record (_ delete) (foo id))\n\n\
   Traversals\n\
   ==========\n\n\
   The expression `(children C)` transforms all the immediate\n\
   sub-expressions of a s-expression according to C and returns the result.\n\n\
  \          (children (rewrite foo bar)) : (foo foo) => (bar bar)\n\n\
   If C fails on any of the children, the failure propagates outward.\n\n\
  \          (children (rewrite foo bar)) : (foo wow) => _|_\n\n\
   This is easily overcome, if desired\n\n\
  \    (children (try (rewrite foo bar))) : (foo wow) => (bar wow)\n\n\
   If the input is atomic (i.e., there are no children), then C is trivially\n\
   successful on *all* the children.\n\n\
  \          (children (rewrite foo bar)) : wow => wow\n\n\
   Top-down and bottom-up iterated traversal strategies are defined recursively\n\
   in terms of children expressions.\n\n\
  \    (topdown C) = (seq C (children (topdown C)))\n\n\
  \    (bottomup C) = (seq (children (bottomup C)) C)\n\n\
   For example:\n\n\
  \     (topdown (try (rewrite a b))) : (a (c a)) => (b (c b))\n\
  \    (bottomup (try (rewrite a b))) : (a (c a)) => (b (c b))\n\n\
   The difference between topdown and bottomup can be seen here:\n\n\
  \    if\n\n\
  \    C = (try (rewrite (not (and $A $B)) (or (not $A) (not $B))))\n\n\
  \    then\n\n\
  \    (topdown C)\n\
  \      : (not (and a (and b c))) => (or (not a) (or (not b) (not c)))\n\n\
  \    but\n\n\
  \    (bottomup C)\n\
  \      : (not (and a (and b c))) => (or (not a) (not (and b c)))\n\n\
   Note that one must take care when using topdown to avoid infinite loops!\n\n\
  \    (topdown (rewrite a (a a))) : a => ...      (never returns!)\n\n\
   Deletion\n\
   ========\n\n\
   `delete` allows one to delete a sexp from its containing sexp.  Semantically,\n\
   it is essentially a special kind of return value that is recognized by other\n\
   constructs, like `children`, and causes them to delete the component sexp.\n\n\
  \    delete : foo => _|_\n\
  \     (children delete) : (foo bar) => ()\n\
  \     (children (alt (rewrite foo 13) delete)) : (foo bar) => (13)\n\n\
   `delete` satisfies the following equalities\n\n\
  \    (seq delete C) == delete\n\
  \    (alt delete C) == delete\n\n\
   Miscellaneous\n\
   =============\n\n\
   `lowercase` does exactly what you would think it does.\n\n\
  \    lowercase : Word        => word\n\
  \    lowercase : UPPERCASE   => uppercase\n\
  \    lowercase : CamelCase   => camelcase\n\
  \    lowercase : (A (B C) D) => (a (b c) d)\n\
  \    lowercase : 1234        => 1234\n\n\
   `concat` concatenates all the atoms of the input expression into a single\n\
   atom.\n\n\
  \    concat : Word          => Word\n\
  \    concat : (' \"A B\" ') => \"'A B'\"\n\
  \    concat : (A (B C) D)   => ABCD\n\n\
   Sub-queries\n\
   ===========\n\n\
   Finally, we have a way to call the query language from the change\n\
   language.  The semantics of `(query Q)` are to gather up all the\n\
   s-expressions output by running the query Q against the input and\n\
   gathering them all up into a single list.\n\n\
  \    (query Q) : S => (X1 ... Xn)    if Q : S => {X1, ... , Xn}\n\n\
   (see the query subcommand's internal documentation for information on\n\
   query semantics).\n\n\
   Note that this operation always succeeds (never results in _|_).\n"
;;

let query_by_example_dot_md =
  "# Sexp query by example\n\n\
   Sexp query is a command-line tool for getting data out of S-expressions. Really it's a\n\
   mini programming language, and if you want to use it effectively you'll want to see \
   lots of\n\
   examples. That's what this README is for.\n\n\
   See also 'sexp pat-query' for a slightly simpler regular-expression-like language \
   that is\n\
   less powerful but can accomplish almost all of the same common tasks.\n\n\
   Table of contents\n\
   =================\n\n\
   - Basic sexp query commands and how they work\n\
  \    - Field\n\
  \    - Index\n\
  \    - Equals and Test\n\
  \    - Pipe\n\
  \    - Each\n\
  \    - Smash\n\
  \    - Regex\n\
  \    - Cat and Wrap\n\
   - \"sexp select\" and \"sexp multi-select\"\n\n\
   Basic sexp query commands and how they work\n\
   ==============================================\n\n\
   The basic building blocks of a sexp query are the commands `field`, `index`, `equals`,\n\
   `test`, `pipe`, `each`, `smash`, `regex`, `cat`, and `wrap`. (There are a few other\n\
   commands but they're less important.) We'll explain each of these in turn before \
   getting\n\
   into more complicated examples.\n\n\
   Field\n\
   -----\n\n\
   `field` gets the value from a `(key value)` pair. Suppose you've got a record with two\n\
   fields, `animal_name` and `age`. Well, calling `(field animal_name)` gets you the\n\
   value of that field:\n\n\
   ```sh\n\
  \  $ echo '((animal_name \"capybara\") \\\\\n\
  \           (age 3))' | sexp query '(field animal_name)'\n\
  \  # => \"capybara\"\n\
   ```\n\n\
   Index\n\
   -----\n\n\
   `index` is a little dumber, in that it just gives you the nth element in a list, \
   indexed\n\
   from 0:\n\n\
   ```sh\n\
  \  $ echo \"(one two three four five)\" | sexp query '(index 2)'\n\
  \  # => three\n\
   ```\n\n\
   If you've got a single-element list, like `(one)`, calling `(index 0)` on it is a nifty\n\
   trick for removing the parens.\n\n\
   `index` also supports negative indexing, allowing you to get elements relative to the \
   end\n\
   of the list rather than to the start:\n\n\
   ```sh\n\
  \  $ echo \"(one two three four five)\" | sexp query '(index -1)'\n\
  \  # => five\n\
   ```\n\n\
   Equals and Test\n\
   ---------------\n\n\
   `equals` and `test` are often used together, because `equals` alone is rarely what you\n\
   want. It just returns a value if the value is equal to some string, and nothing \
   otherwise.\n\
   Suppose we had a little corporate directory expressed as an S-expression (or really, a\n\
   series of expressions, each separated by a blank line):\n\n\
   ```ocaml\n\
  \  ;; ./corpdir.sexp\n\n\
  \  ((name ((first Bill) (last Nye)))\n\
  \   (role \"Science guy\")\n\
  \   (start_date ((year 2012) (month 3) (day 4))))\n\n\
  \  ((name ((first Zadie) (last Smith)))\n\
  \   (role \"Author\")\n\
  \   (start_date ((year 2016) (month 10) (day 21))))\n\
   ```\n\n\
   Now we can run some `equals` checks against it. Notice below how the `equals` \
   expression\n\
   follows the `(field role)` expression; that's because it's operating on the output of\n\
   `(field role)`, as if piping the results of the first command into the second. (We'll \
   see\n\
   how this works under the hood once we get to the `pipe` command.)\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(field role) (equals \"Author\")'\n\
  \  # => Author\n\n\
  \  $ cat corpdir.sexp | sexp query '(field role) (equals \"Science guy\")'\n\
  \  # => \"Science guy\"\n\n\
  \  $ cat corpdir.sexp | sexp query '(field role) (equals \"Foo\")'\n\
  \  # =>\n\
   ```\n\n\
   You can see why this isn't very useful: in general when we test a record for \
   something, we\n\
   want to *do* something with that record. But here we just return the value we're \
   testing\n\
   against. This is where `test` comes in:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(test (field role) (equals \"Author\"))'\n\
  \  # => ((name ((first Zadie) (last Smith))) (role Author)\n\
  \  # => (start_date ((year 2016) (month 10) (day 21))))\n\n\
  \  $ cat corpdir.sexp | sexp query \\\\\n\
  \      '(test (field role) (equals \"Author\")) (field start_date)'\n\
  \  # => ((year 2016) (month 10) (day 21))\n\
   ```\n\n\
   `test` wraps around a condition, like `(equals 'foo')`, and if the condition passes, it\n\
   returns the entire S-expression satisfying the condition. You can then pass this to the\n\
   right -- here, to the `(field start_date)` operator -- to get at some field within that\n\
   returned value. A lot of sexp queries work this way: first you filter for records\n\
   satisfying a condition, and then you dig in to those looking for specific data.\n\n\
   Pipe\n\
   ----\n\n\
   `pipe` gets its name from the Unix command-line `|` that passes output from one \
   program to\n\
   another. It's a way of chaining together a sequence of commands, like a\n\
   `Sequence.concat_map` for sexp queries.\n\n\
   In many cases you don't actually have to write the pipe, because it's already there\n\
   implicitly. For instance this query:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(field start_date) (field day)'\n\
  \  # => 4\n\
  \  # => 21\n\
   ```\n\n\
   takes the result of the first command, `(field start_date)`, and implicitly pipes it to\n\
   the next, `(field day)`. It's as if there's a literal `|` pipe character between the \
   two\n\
   statements. Writing it with the actual pipe command gives:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(pipe (field start_date) (field day))'\n\
  \  # => 4\n\
  \  # => 21\n\
   ```\n\n\
   One pipe command can take an arbitrary number of sub-statements, not just 2, as in the\n\
   following example:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(pipe (field start_date) (field day) (equals 4))'\n\
  \  # => 4\n\
   ```\n\n\
   It's worth asking: if there's syntactic sugar to get rid of these explicit pipes, do \
   you\n\
   ever really need the `pipe` command? In fact you do. While some commands, like \
   `test`, can\n\
   take a series of substatements without requiring a pipe, others, like `cat` and \
   `unquote`,\n\
   which we'll see later, require it. So you'll see `pipe` all over, usually in places \
   where\n\
   you have a complex sub-query, i.e., a query that involves more than a single `(field \
   ...)`\n\
   command.\n\n\
   Each\n\
   ----\n\n\
   `each` is pretty simple: it takes each element in a list and passes them one by one to\n\
   another expression. At the top level, you can use it like:\n\n\
   ```sh\n\
  \  <list> each <expression>\n\
   ```\n\n\
   as in the following example:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query \\\\\n\
  \      '(test (field role) (equals \"Science guy\")) (field name) each (index 1)'\n\
  \  # => Bill\n\
  \  # => Nye\n\
   ```\n\n\
   It's worth dwelling a bit on what's happening here. On the left-hand side of the \
   `each`,\n\
   you have an expression that returns the `name` field of the record where the role field\n\
   has the value \"Science guy\". So what you're passing to the `each` is the list:\n\n\
   ```sh\n\
  \  ((first Bill) (last Nye))\n\
   ```\n\n\
   which has two elements. (Notice how our little command-line program returns two lines.)\n\
   Then, the right-hand side of the `each` is just an expression that operates on each\n\
   element of the list, so on `(first Bill)` and `(last Nye)` in turn. `(index 1)` returns\n\
   the second element of whatever it's passed, which is how we end up with \"Bill\" and \
   \"Nye\".\n\n\
   (`each` appears to be an infix operator because of the implicit pipe\n\
   at the top level. But if you were to use it inside of a `test`, for\n\
   example, as in `(test (pipe (field hosts) each)`, you must explicitly\n\
   pipe the output of `field` to the `each`.)\n\n\
   Smash\n\
   -----\n\n\
   smaaaaasssshhhh!!!! This one has the coolest name, and also, in a way, the coolest\n\
   behavior: It takes an S-expression and returns every sub-expression of it. Then, like\n\
   `each`, it lets you apply a command to every one of those sub-expressions. So it also \
   uses\n\
   that infix-style `<expression> smash <expression>` syntax. But let's see what it looks\n\
   like when we operate on the whole _corpdir.sexp_ file, without actually doing anything\n\
   with the smashed contents:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query 'smash'\n\
  \  # => ((name ((first Bill) (last Nye))) (role \"Science guy\")\n\
  \  # =>  (start_date ((year 2012) (month 3) (day 4))))\n\
  \  # => (name ((first Bill) (last Nye)))\n\
  \  # => name\n\
  \  # => ((first Bill) (last Nye))\n\
  \  # => (first Bill)\n\
  \  # => first\n\
  \  # => Bill\n\
  \  # => (last Nye)\n\
  \  # => last\n\
  \  # => Nye\n\
  \  # => (role \"Science guy\")\n\
  \  # => role\n\
  \  # => \"Science guy\"\n\
  \  # => (start_date ((year 2012) (month 3) (day 4)))\n\
  \  # => start_date\n\
  \  # => ((year 2012) (month 3) (day 4))\n\
  \  # => (year 2012)\n\
  \  # => year\n\
  \  # => 2012\n\
  \  # => (month 3)\n\
  \  # => month\n\
  \  # => 3\n\
  \  # => (day 4)\n\
  \  # => day\n\
  \  # => 4\n\
  \  #\n\
  \  # => ((name ((first Zadie) (last Smith))) (role Author)\n\
  \  # =>  (start_date ((year 2016) (month 10) (day 21))))\n\
  \  # => (name ((first Zadie) (last Smith)))\n\
  \  # => name\n\
  \  # => ((first Zadie) (last Smith))\n\
  \  # => (first Zadie)\n\
  \  # => first\n\
  \  # => Zadie\n\
  \  # => (last Smith)\n\
  \  # => last\n\
  \  # => Smith\n\
  \  # => (role Author)\n\
  \  # => role\n\
  \  # => Author\n\
  \  # => (start_date ((year 2016) (month 10) (day 21)))\n\
  \  # => start_date\n\
  \  # => ((year 2016) (month 10) (day 21))\n\
  \  # => (year 2016)\n\
  \  # => year\n\
  \  # => 2016\n\
  \  # => (month 10)\n\
  \  # => month\n\
  \  # => 10\n\
  \  # => (day 21)\n\
  \  # => day\n\
  \  # => 21\n\
   ```\n\n\
   What's going on here? Well, since we passed the whole file to `smash`, rather than \
   just a\n\
   single record, we're getting the smashed contents of each of our two records in turn \
   (one\n\
   for Bill Nye and one for Zadie Smith). For each of these, the command is coughing up \
   every\n\
   sub-expression of the original record. You can think of it as taking anything of the \
   form\n\
   `<left> <right>` and printing `<left> <right>`, `<left>`, and `<right>`. Since\n\
   S-expressions can be deeply nested, this can end up printing a lot of stuff.\n\n\
   Smashing is useful when you don't want to do a million chained tests in order to get to\n\
   some record nested deep in an S-expression. For instance, suppose our day records were\n\
   buried in a lot of other stuff, like so:\n\n\
   ```ocaml\n\
  \  ;; ./corpdir.sexp\n\n\
  \  ((name ((first Bill) (last Nye)))\n\
  \   (role \"Science guy\")\n\
  \   (start_date ((year 2012) (month 3) (period ((unit ((kind ((sol 400) (day \
   4))))))))))\n\n\
  \  ((name ((first Zadie) (last Smith)))\n\
  \   (role \"Author\")\n\
  \   (start_date ((year 2016) (month 10) (period ((unit ((kind ((sol 2100) (day \
   21))))))))))\n\
   ```\n\n\
   If you knew you wanted to get at those `(day <num>)` records, you could write something\n\
   like:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(field start_date) (field period) (field unit) \\\\\n\
  \                                     (field kind) each (test (index 0) (equals day))'\n\
  \  # => (day 4)\n\
  \  # => (day 21)\n\
   ```\n\n\
   or... you could just smash the input and filter on the field name:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query 'smash (test (index 0) (equals day))'\n\
  \  # => (day 4)\n\
  \  # => (day 21)\n\
   ```\n\n\
   Keep in mind that using `smash` isn't free: there's a tradeoff between being concise \
   and\n\
   being precise when deciding whether to use it. That is, while it may be powerful for\n\
   finding deeply nested things, that's only because you've given up some control over \
   where\n\
   the thing is to be found. In a way, `smash` is the sexp query analog of `.*` in regular\n\
   expressions. It should be used with caution.\n\n\
   Regex\n\
   -----\n\n\
   `regex` is like `equals`, except that instead of taking a simple string argument, it \
   takes\n\
   a regular expression, so that you can do slightly more versatile searching. Let's say \
   we\n\
   had a new hire in our corpdir:\n\n\
   ```ocaml\n\
  \  ;; ./corpdir.sexp\n\n\
  \  ((name ((first Bill) (last Nye)))\n\
  \  (role \"Science guy\")\n\
  \  (start_date ((year 2012) (month 3) (day 4))))\n\n\
  \  ((name ((first Zadie) (last Smith)))\n\
  \  (role \"Author\")\n\
  \  (start_date ((year 2016) (month 10) (day 21))))\n\n\
  \  ((name ((first David) (last Lynch)))\n\
  \  (role \"Auteur\")\n\
  \  (start_date ((year 2017) (month 5) (day 20))))\n\
   ```\n\n\
   If we then wanted to get the name records of everyone whose role starts with \"Au\", we\n\
   could use regex to do it:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(test (field role) (regex \"^Au\")) (field name)'\n\
  \  # => ((first Zadie) (last Smith))\n\
  \  # => ((first David) (last Lynch))\n\
   ```\n\n\
   By default, `regex` will return the entire string if there's a match, and nothing if \
   not;\n\
   but if you use a capture group, as in the following example, it'll return the capture\n\
   group's contents instead. (If you supply multiple capture groups it'll return the \
   result\n\
   of the first one.) For instance:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(field role) (regex \"^Au(.*)\")'\n\
  \  # => thor\n\
  \  # => teur\n\
   ```\n\n\
   Cat and Wrap\n\
   ------------\n\n\
   `cat` is how you run multiple commands on a single S-expression at one time,\n\
   con-`cat`-enating the results. Where `pipe` is a way of combining sub-queries in \
   series,\n\
   `cat` combines them in parallel. So for example:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(cat (field name) (field role))'\n\
  \  # => ((first Bill) (last Nye))\n\
  \  # => \"Science guy\"\n\
  \  # => ((first Zadie) (last Smith))\n\
  \  # => Author\n\
  \  # => ((first David) (last Lynch))\n\
  \  # => Auteur\n\
   ```\n\n\
   Notice how for each record we've fetched both the name and the role. But also notice \
   how\n\
   the results aren't wrapped up into a single S-expression. That's where `wrap` comes \
   in --\n\
   it's a command that simply takes some stuff and wraps it in parens, and it's frequently\n\
   used together with `cat`:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(wrap (cat (field name) (field role)))'\n\
  \  # => (((first Bill) (last Nye)) \"Science guy\")\n\
  \  # => (((first Zadie) (last Smith)) Author)\n\
  \  # => (((first David) (last Lynch)) Auteur)\n\
   ```\n\n\
   Now the results of our multiple queries are nicely wrapped up into a single \
   S-expression\n\
   per record.\n\n\
   `sexp select` and `sexp multi-select`\n\
   =====================================\n\n\
   A lot of the time, what would be a fairly complicated sexp query is more easily \
   expressed\n\
   as a sexp multi-select. Suppose you wanted to pull out the actual day, month, and \
   year of\n\
   each person in our little corpdir. You could do it using sexp query:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp query '(field start_date) \\\\\n\
  \                                     (wrap (cat (field day) (field month) (field \
   year)))'\n\
  \  # => (4 3 2012)\n\
  \  # => (21 10 2016)\n\
  \  # => (20 5 1999)\n\
   ```\n\n\
   But it's actually much easier when expressed as a multi-select:\n\n\
   ```sh\n\
  \  $ cat corpdir.sexp | sexp multi-select day month year\n\
  \  # => (4 3 2012)\n\
  \  # => (21 10 2016)\n\
  \  # => (20 5 1999)\n\
   ```\n\n\
   This multi-select does a kind of `smash`, `cat`, and `wrap` on the fields that you \
   pass to\n\
   it. Notice that you don't even need to quote your field names!\n\n\
   Because it's more or less doing a smash, the same caveat applies: multi-select is a\n\
   concise way to find things, but at the expense of being somewhat imprecise about where\n\
   you're looking.\n"
;;

let query_semantics_dot_md =
  "# Sexp query formal semantics\n\n\
   See also the get and select and pat-query subcommands.\n\n\
   This document explains the semantics of query expressions, whose\n\
   syntax is as follows:\n\n\
  \    E ::=\n\
  \        -- selection -------------\n\
  \        | (index NUM)\n\
  \        | (field STRING)\n\
  \        | each\n\
  \        | smash\n\
  \        -- composition -----------\n\
  \        | (pipe E E ...)\n\
  \        | (cat E E ...)\n\
  \        | this\n\
  \        | none\n\
  \        -- tests/conditionals ----\n\
  \        | atomic\n\
  \        | (variant TAG NUM)\n\
  \        | (equals SEXP ...)\n\
  \        | (regex R)\n\
  \        | (test E E ...)\n\
  \        | (not E)\n\
  \        | (and E E ...)\n\
  \        | (or E E ...)\n\
  \        | (if E E E)\n\
  \        | (branch E E E)\n\
  \        -- formatting ------------\n\
  \        | (wrap E)\n\
  \        | (quote T[0])\n\
  \        -- transformations -------\n\
  \        | (change C)\n\
  \        | restructure\n\
  \        | length\n\n\
   The meaning of an expression is a function from an input s-expression to a\n\
   (possibly empty) sequence of output s-expressions.  The toplevel command is\n\
   applied to each expression in the input.  We will look at five categories\n\
   of expressions: selection, composition, conditionals, formatting, and\n\
   transformation.\n\n\
   Selection\n\
   ---------\n\n\
   An `(index N)` expression picks out the Nth element of a list s-expression, or the\n\
   Nth-from-last element if N is negative. It succeeds if N is a valid index (i.e., -(list\n\
   length) <= N < list length) and fails otherwise. Upon success, it returns the\n\
   single-element sequence containing the selected sub-expressions. Upon failure, it \
   returns\n\
   an empty list.\n\n\
  \    (index 2)  : (one two three four) => {three}\n\
  \    (index 8)  : (one two three four) => {}\n\
  \    (index -1) : (one two three four) => {four}\n\
  \    (index -5) : (one two three four) => {}\n\n\
   A `(field F)` expression is like `(index N)` except that it projects\n\
   the field named F out of a record.  If more than one field of that\n\
   name exists, they will all be selected.\n\n\
  \    (field foo) : ((bar 1) (foo 2) (baz 3))         => {2}\n\
  \    (field foo) : ((bar 1) (foo 2) (baz 3) (foo 4)) => {2, 4}\n\
  \    (field wow) : ((bar 1) (foo 2) (baz 3))         => {}\n\n\
   An `each` expression selects every element of a list.\n\n\
  \    each : (one two three four) => {one, two, three, four}\n\
  \    each : ()    => {}\n\
  \    each : hello => {}\n\n\
   A `smash` expression selects every sub-expression of an s-expression\n\n\
  \    smash : (a (b c) (d (e f))) => { (a (b c) (d (e f))),\n\
  \                                      a, (b c), (d (e f)),\n\
  \                                          b, c,  d, (e f),\n\
  \                                                     e, f }\n\n\
   Composition\n\
   -----------\n\n\
   The two basic composition operators are `pipe` and `cat`.  These two\n\
   operators do sequential and parallel composition, respectively.  The\n\
   denotation of\n\n\
  \                    (pipe E1 E2 ... En)\n\n\
   is a \"fanned out\" composition of the denotations D = [E1] and\n\
   D' = [`(pipe E2 ... En)`] in which D' is called on every s-expression\n\
   in the output sequence of D and all the resulting sequences of\n\
   s-expressions are concatenated together into one big sequence.\n\n\
  \            (pipe E) = E\n\
  \            (pipe E1 E2 ... En) : S => X1 U ... U Xm\n\n\
  \            where   E1 : S => {S1, ..., Sm}\n\
  \              and   (pipe E2 ... En) : Si => Xi   for i in {1..m}\n\n\
   In particular, this means that if D returns an empty sequence, then\n\
   D' will never be called.  The denotation of\n\n\
  \                    (cat E1 E2 ... En)\n\n\
   is simply a parallel execution of each Ei on the input X in which all\n\n\
  \            (cat E1 ... En) : S => X1 U ... U Xn\n\n\
  \            where   Ei : S => Xi    for i in {i .. n}\n\n\
   There are two trivial expressions, `this` and `none`.  A this expression\n\
   simply selects it input, while a `none` expression selects nothing\n\n\
  \                this : S => {S}         none : S => {}\n\n\
   These are identities for the pipe and cat operators.\n\n\
  \                this = (pipe)          none = (cat)\n\n\
   (For monad fans -- pipe is Kleisli composition and `this` is Kleisli\n\
   identity for the list monad.)\n\n\n\
   Conditionals\n\
   ------------\n\n\
   For the purpose of conditional execution, we treat an expression returning\n\
   a non-empty sequence as true, and an expression returning an empty sequence\n\
   as false.  With this in mind, one may think of `this` and `none` as the\n\
   constants true and false, respectively.\n\n\
   A `atomic` expression tests for atomic s-expressions.\n\n\
  \        atomic : foo       => {foo}\n\
  \        atomic : (foo bar) => {}\n\n\
   The most basic non-trivial condition is equality.  The expression\n\
   `(equals S)` selects its input in the case that the (returns true) and\n\
   fails (returns false) otherwise.\n\n\
  \        (equals S) : S' => {S'}    if S = S'\n\
  \        (equals S) : S' => {}      otherwise\n\n\
   There is also a N-ary version of equals that is expands to a disjunction\n\n\
  \        (equals S1 S2 ...) = (or (equals S1) (equals S2) ...)\n\n\
   A `(regex R)` expression tests for the s-expresion to be an ATOM that\n\
   matches R.  The truth value will be the first capturing group of the\n\
   regular expression or the whole atom if regular expression has no\n\
   capturing groups.\n\n\
   A `(variant F N)` expression tests for the s-expression form of an element\n\
   of a variant type with constructor F applied to N arguments.  You can leave N off, in\n\
   which case it will match a variant with any number of arguments.\n\n\
  \    (variant foo 5) : (foo 1 2 3 4 5) => {(foo 1 2 3 4 5)}\n\
  \    (variant foo 3) : (foo 1 2 3 4 5) => {}\n\
  \    (variant foo 8) : (foo 1 2 3 4 5) => {}\n\
  \    (variant bar 5) : (foo 1 2 3 4 5) => {}\n\
  \    (variant foo 0) : foo => {foo}\n\
  \    (variant foo 0) : (foo) => {(foo)}\n\
  \    (variant foo)   : (foo 1 2 3 4 5) => {(foo 1 2 3 4 5)}\n\
  \    (variant foo)   : foo => {foo}\n\
  \    (variant foo)   : (foo) => {(foo)}\n\n\
   A `(test E)` expression proceeds by evaluating E on the current s-expression\n\
   S and selecting S only in the case that E succeeds.\n\n\
  \        (test E) : S => {}    if X empty      where  E : S => X\n\
  \        (test E) : S => {S}   otherwise\n\n\
   We also provide `(test E1 E2 ...)` as syntactic sugar for the common\n\
   idiom `(test (pipe E1 E2 ...))`.\n\n\
   A `(not E)` expression proceeds by evaluating E on the current s-expression\n\
   S and selecting S only in the case that E fails.\n\n\
  \        (not E) : S => {S}    if X empty      where  E : S => X\n\
  \        (not E) : S => {}     otherwise\n\n\
   Note that X is is discarded from the output of both (test E) and (not E).\n\
   For this reason, these two operators are useful for \"looking down\" into an\n\
   s-expression while remembering your place at some point above where you will\n\
   return later.\n\n\
   An `(and E1 ... En)` expression does short-circuit evaluation based on whether\n\
   or not E1 succeeds.  Upon success, it returns the results of En.\n\n\
  \    (and) = this\n\n\
  \    (and E) = E\n\n\
  \    (and E1 E2 ... En) : S => {}        if  E1 : S => {}\n\
  \    (and E1 E2 ... En) : S => Y         if  E1 : S => X  (nonempty)\n\
  \                                        and (and E2 ... En) : S => Y\n\n\
   An `(or E1 ... En)` expression does short-circuit evaluation based on whether\n\
   or not E1 succeeds.  It returns the results of the first Ei that succeeds.\n\n\
  \    (or) = none\n\n\
  \    (or E) = E\n\n\
  \    (or E1 E2 ... En) : S => X         if  E1 : S => X  (nonempty)\n\
  \    (or E1 E2 ... En) : S => Y         if  E1 : S => {}\n\
  \                                       and (or E2 ... En) : S => Y\n\n\
   An `(if E1 E2 E3)` expression does conditional execution of E2 or E3 based\n\
   on whether or not E1 succeeds.\n\n\
  \    (if E1 E2 E3) : S => X2      if E1 : S => X (non-empty) and\n\
  \    (if E1 E2 E3) : S => X3      if E1 : S => {}\n\n\
  \       where   Ei : Si => Xi    for i in {2,3}\n\n\
   An `(branch E1 E2 E3)` expression does conditional execution like `if`, but\n\
   also pipes the output of the condition into the `then' branch.\n\n\
  \    (branch E1 E2 E3) : S => X1 U ... U Xn\n\n\
  \                                    if E1 : S => {S1, ... , Sn} (non-empty)\n\
  \                                    and E2 : Si => Xi  for i in {1..n}\n\n\
  \    (branch E1 E2 E3) : S => X3\n\
  \                                    if E1 : S => {}\n\
  \                                    and E3 : S3 => X3\n\n\
   The following equations relating the behavior of `if` and `branch` are true:\n\n\
  \    (if E1 E2 E3) = (branch (test E1) E2 E3)\n\n\
  \    (branch E1 E2 E3) = (if E1 (pipe E1 E2) E3)\n\n\n\
   Formatting\n\
   ----------\n\n\
   Using the commands so far, one may only output sub-expressions of the input\n\
   expression itself.  However, we may also want to impose additional structure\n\
   on the output.\n\n\
   A `(wrap E)` expression runs E and gathers up the resulting sequence into a\n\
   single list s-expression that becomes the (single) overall result.\n\n\
  \    (wrap E) : S => {(S1 ... S2)}  where  E : S => {S1, ... , S2}\n\n\
   Note that the final output sequence has exactly one element that is a list.\n\n\
   A `(quote S)` expression adds the provided s-expression to the manifest.\n\n\
  \    (quote S') : S => {S'}\n\n\
   Note that S is discarded here.  For this reason, quote is often used in\n\
   conjunction with a parallel composition operator like `cat`, `and`, or `or`.\n\n\
  \    (quote (a b c))              : (1 2 3) => {(a b c)}\n\
  \    (quote (a (unquote each) c)) : (1 2 3) => {(a 1 c), (a 2 c), (a 3 c)}\n\
  \    (quote (a (splice each) c))  : (1 2 3) => {(a 1 2 3 c)}\n\n\
  \    (quote (a (splice each) c (unquote each)))\n\
  \        : (1 2 3) => {(a 1 2 3 c 1), (a 1 2 3 c 2), (a 1 2 3 c 3)}\n\n\
   Multiple unquotes in a single quoted template yield a cartesian product.\n\n\
  \    (quote (a (unquote (pipe (index 0) each))\n\
  \            b (unquote (pipe (index 1) each))))\n\
  \        : ((1 2 3) (x y z)) => {(a 1 b x), (a 1 b y), (a 1 b z),\n\
  \                                (a 2 b x), (a 2 b y), (a 2 b z),\n\
  \                                (a 3 b x), (a 3 b y), (a 3 b z)}\n\n\
   Furthermore, nested quotes increase the \"degree\" of quotation.  Splice and\n\
   unquote only have their effect at the degree zero.  This feature is intended\n\
   to facilitate sexpquery expressions that manipulate other sexpquery expressions.\n\n\
   --- grammar for sexpquery templates ---\n\n\
  \          T[0] ::= ATOM               T[n+1] ::= ATOM\n\
  \                 | (T[0] ... T[0])             | (T[n+1] ... T[n+1])\n\
  \                 | (quote T[1])                | (quote T[n+2])\n\
  \                 | (unquote E)                 | (unquote T[n])\n\
  \                 | (splice E)                  | (splice T[n])\n\n\
   Transformations\n\
   ---------------\n\n\
   Finally, we have a way to call the change language from the query\n\
   language.  The semantics of (change C) are to return the transformed\n\
   expression as a singleton sequence and propagate failure.\n\n\
  \    (change C) : S => {S'}    if C : S => S'\n\
  \    (change C) : S => {}      if C : S => _|_\n\n\
   (see the change subcommand's internal documentation for information on\n\
   change semantics).\n\n\
   Sometimes the contents of an atom will be the string representation of\n\
   a sequence of sexps. `restructure` will do this interpretation for you:\n\n\
  \    restructure : \"A (B C) D\" => {A (B C) D}\n\n\
   Sometimes you want the number of elements in a list.\n\
  \    \n\
  \    length : ATOM => {1}\n\
  \    length : (T[1] ... T[N]) => {N}\n"
;;

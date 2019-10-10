let change_by_example_dot_md =
  "# Sexp change by example\n\nSexp change is a command-line tool for manipulating S-\
   expressions,\nuseful if you want to do some data munging without having to write \
   a\nfull-fledged program to parse and process your sexps.\n\nThis doc collects examp\
   les of sexp change in action. For a more\ncomplete overview of the language, see\n\
   [this doc](./change_semantics.md),\nwhich includes many small examples showing ho\
   w to use each keyword.\n\nApplying a rewrite rule throughout a file\n--------------\
   ---------------------------\n\nPerhaps the most common way to use `sexp change` is\
  \ just to barrel\nthrough a file attempting to apply a rewrite rule. You can accom\
   plish\nthis with the following:\n\n```\n(topdown (try (rewrite LHS RHS)))\n```\n\nThe `\
   topdown` keyword ensures you cover the entire file, while `try`\nensures that if \
   you can't apply the rewrite rule to a given expression\nLHS, you leave LHS as it \
   is.\n\nFor convenience, this functionality is exposed with the `sexp rewrite` comm\
   and.\n\nUsing sexp query and sexp change together\n--------------------------------\
   ---------\n\nHere are a pair of examples making use of sexp query's `index`,\n`pipe\
   `, `quote` and `unquote` commands, and sexp change's `seq`,\n`rewrite`, and `conc\
   at` commands.\n\nSuppose your input is something like:\n\n```\n(AMZN ( ... ))\n(MSFT (\
  \ ... ))\n```\n\nand you want to change it to:\n\n```\n(\"AMZN US\" ( ... ))\n(\"MSFT US\" (\
  \ ... ))\n```\n\nHere's how you could do it using the `change` subcommand of `sexp q\
   uery`:\n\n```sh\nsexp query '\n(quote (\n  (unquote (pipe (index 0) (change (seq (rew\
   rite $X ($X \" US\")) concat))))\n  (unquote (index 1))))\n' <<EOF\n(AMZN (foo bar))\n\
   (MSFT (foo bar))\nEOF\n```\n\nLet's break down how this works. From\n[the sexp query \
   readme](query_by_example.md) you know how to use\n`quote` to create an S-expressi\
   on \"template\" that is then filled in\nwith `unquote` statements. Here, that templ\
   ate is going to look like\n`(A B)`, where the `A` part is filled in by the result\
   s of:\n\n```sh\n(pipe (index 0) (change (seq (rewrite $X ($X \" US\")) concat)))\n```\n\
   \nWe pipe the results of `(index 0)`, i.e., something like \"AMZN\", to\nthe `change\
   ` command, which consists of a `seq`-uence of two\nsubcommands: `(rewrite $X ($X \
   \" US\"))` takes the single S-expression\nlike `AMZN` (an atom) and rewrites it as \
   `(AMZN \" US\")`. `concat` then\nconcatenates this 2-atom list into the single atom\
  \ `\"AMZN US\"`.\n\nFinally, the second half of the `quote` statement plops the resul\
   t of\n`(unquote (index 1))` into the latter half of the list, creating the\ndesire\
   d `(AMZN (foo bar))`.\n\nSuppose we wanted to do this for sexps of more than two i\
   tems, like\nso:\n\n```\n(AMZN ( ... ) ( ... ))\n(MSFT ( ... ) ( ... ) ( ... ))\n```\n\nT\
   hen you'd want to use the `query` subcommand of `sexp change`:\n\n```sh\nsexp chang\
   e '\n(seq\n  (rewrite ($A @B) ($A (@B)))\n  (query (\n    quote (\n      (unquote (pi\
   pe (index 0) (change (seq (rewrite $X ($X \" US\")) concat))))\n      (unquote (ind\
   ex 1)))))\n  (rewrite (($A (@B))) ($A @B)))\n' <<EOF\n(AMZN (foo bar) baz)\n(MSFT (f\
   oo bar))\nEOF\n```\n\nNotice how the inner part of this command is the exact same `q\
   uery`\nwe just did. But that's step 2 of a sequence of three steps, the first\nof \
   which is to wrap the \"tail\" of our list in parens, via:\n\n```sh\n(rewrite ($A @B) \
   ($A (@B)))\n```\n\nand then, after we're done with the meat of the rewrite, unwrap \
   it\nagain via the inverse:\n\n```sh\n(rewrite (($A (@B))) ($A @B))\n```\n\nwhere the ex\
   tra parens around `(($A (@B)))` are added because we're\ninside a `sexp change`.\n\
   \nOverwriting all record values with a given key\n--------------------------------\
   --------------\n\nSee the following little shell command:\n\n```sh\nfunction proxy-ir\
   on-config-command {\n    no-args \"$@\"\n    host=james\n    sexp change </etc/iron-c\
   onfig.sexp \\\n         \"(record (host (const $host)))\"\n}\n```\n\nThis operates on a \
   file with S-expressions of the form:\n\n```ocaml\n((host some_host_name)\n (async_rp\
   c_port (Static 7013))\n (hgrc /path/to/hgrc)\n (serializer_pause_timeout 2m)\n (rpc\
   _proxy_config\n  ((another_host1 (p1 p2 p3))\n   (another_host2 (p1 p2)))))\n```\n\ni\
   .e., records with keys `host`, `async_rpc_port`, `hgrc`,\n`serializer_pause_timeo\
   ut`, and so on. By writing\n`(record (host (const $host)))`, the above sexp chang\
   e program just\ntakes whatever the value of the interpolated `$host` ends up bein\
   g --\nthis is a shell variable, not a variable in the sexp change language\n-- and\
  \ jams it into the place of _every_ `host` record value. So if\n`$host` were \"foo\"\
   , the above record would be rewritten as:\n\n```ocaml\n((host foo)\n (async_rpc_port\
  \ (Static 7013))\n (hgrc /path/to/hgrc)\n (serializer_pause_timeout 2m)\n (rpc_proxy\
   _config\n  ((another_host1 (p1 p2 p3))\n   (another_host2 (p1 p2)))))\n```\n\nand so \
   on for the rest of the records in the file -- they would all\nbegin `(host foo)`.\
   \n\nDoing a bottom-up search with alt and try\n------------------------------------\
   -----\n\nHere, we use the `alt` keyword for the simple reason that it allows\nus to\
  \ apply several different rewrite rules in parallel, in a single\npass through a f\
   ile:\n\n```\nsexp change '(bottomup (try (alt\n    (rewrite (App $X $Y) ($X $Y))\n   \
  \ (rewrite (Name $X) $X)\n    (rewrite (Var (Free $X)) $X)\n  )))'\n```\n\nThe `alt` k\
   eyword is short-circuiting, in the sense that it returns\nas soon as one of the r\
   ewrite rules you pass it succeeds. Here, we don't\nreally take advantage of that \
   short-circuiting: no matter what order\nwe put our three rules in, the program wi\
   ll have the same behavior.\n\nIf, however, the LHS of our alternative rewrite rule\
   s had the same\nstructure -- like if they all looked like `(Foo $X) RHS` or `(Foo\
  \ $X)\nRHS'` -- then there would at least be the chance that the order of\nthe rule\
   s could matter.\n"
;;

let change_semantics_dot_md =
  "# Sexp change formal semantics\n\nThis document explains the semantics of change e\
   xpressions, whose\nsyntax is as follows:\n\n     C ::= (rewrite P P)\n         | (se\
   q C C ...)\n         | (alt C C ...)\n         | id\n         | fail\n         | del\
   ete\n         | (const SEXP)\n         | (try C)\n         | (record (F C) ...)\n   \
  \      | (children C)\n         | (topdown C)\n         | (bottomup C)\n         | l\
   owercase\n         | concat\n         | (query Q)\n\n    F ::= ATOM\n\nThe meaning of \
   an expression is a partial function on s-expressions.\nThe toplevel command is ap\
   plied to each input expression in the input.\n(NOTE: this fragment of the languag\
   e is essentially a fragment of Stratego\nterm rewriting language: http://stratego\
   xt.org)\n\nRewriting\n=========\n\nA `(rewrite LHS RHS)` attempts to rewrite an expre\
   ssion by the specified\nrewrite rule.  It first matches the input s-expression ag\
   ainst the\nLHS pattern.  If this succeeds, it instantiates the RHS pattern with\nb\
   indings inferred from the match.\n\nThe syntax of the LHS and RHS patterns is as f\
   ollows:\n\n     P ::= ATOM | (P ... P) | $VAR | @VAR\n\nNote that variables are dist\
   inguished from atoms by a prefix of `$` or `@`.\nVariables starting with `$` are \
   scalar and they match a single s-expression,\nwhile those starting with `@` match\
  \ a list of s-expressions.\n\nSome examples\n\n             (rewrite foo bar) : foo =\
   > bar\n             (rewrite foo bar) : abc => _|_          (indicates failure)\n \
  \            (rewrite foo bar) : (foo bar) => _|_\n       (rewrite (foo bar) wow) \
   : (foo bar) => wow\n         (rewrite (foo $X) $X) : (foo bar) => bar\n         (r\
   ewrite (foo $X) $X) : (foo (bar none)) => (bar none)\n    (rewrite (foo $X) ($X $\
   X)) : (foo bar) => (bar bar)\n       (rewrite (foo @X) (@X)) : (foo bar baz) => (\
   bar baz)\n       (rewrite (foo @X) (@X)) : (foo (bar a) (baz b)) => ((bar a) (baz\
  \ b))\n    (rewrite (foo @X) (@X @X)) : (foo bar baz) => (bar baz bar baz)\n\nSome r\
   ewrite rules are not well formed.  The following rules apply:\n\n    well-formedne\
   ss rule                          non-conforming program\n    --------------------\
   ---------------------------------------------------\n    variables may not be bou\
   nd more than once     (rewrite (foo $X $X) who)\n    all RHS variables must be bo\
   und on LHS        (rewrite (foo bar) (yo $X))\n    no two list variables can be i\
   n the same list (rewrite (foo @X @Y) @X)\n\n`(const SEXP)` is syntactic sugar for \
   `(rewrite $_ SEXP)`.  That is, const is a\nrewrite rule that always succeeds and \
   produces the same sexp.\n\n`(rewrite_record LHS RHS)` should be used if order of s\
   -expressions in lists\nshould be disregarded.  Some examples\n\n    (rewrite_record\
  \ (foo bar) wow) : (bar foo) => wow\n    (rewrite_record (foo bar) wow) : (foo bar\
   ) => wow\n    (rewrite_record (foo bar) wow) : (foo) => _|_\n    (rewrite_record (\
   foo bar) wow) : (bar) => _|_\n    (rewrite_record (bar @X) (wow @X)) : (foo bar b\
   az) => (wow foo baz)\n\nComposition\n===========\n\nThe two basic composition operato\
   rs are `seq` and `alt`.  These two\noperators provide sequencing and biased choic\
   e, respectively.  The\ndenotation of\n\n                    (seq C1 C2 ... Cn)\n\nis \
   a composition of the denotations D = [C1] and D' = [(seq C2 ... Cn)].\n\n    (seq \
   C1 C2 ... Cn) : S => S''    if C1 : S => S'\n                                   a\
   nd (seq C2 ... Cn) : S' => S''\n\nFailure at any point in the sequence propagates \
   out to the whole.\n\n    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => _|_\n\n    (s\
   eq C1 C2 ... Cn) : S => _|_    if C1 : S => S'\n                                 \
  \  and (seq C2 ... Cn) : S' => _|_\n\nUnary application of seq has no effect\n\n    (\
   seq C) = C\n\nThe denotation of\n\n                    (alt C1 C2 ... Cn)\n\nis simply\
  \ a biased choice of whichever of C1 ... Cn first succeeds on\nthe input.\n\n    (al\
   t C1 C2 ... Cn) : S => S'   if C1 : S => S'\n\n    (alt C1 C2 ... Cn) : S => R    \
   if C1 : S => _|_ and (alt C2 ... Cn) : S => R\n                                 (\
   where R is the either _|_ or an s-expression)\n\nThere are two trivial expressions\
   , `id` and `fail`.  A id expression\nsimply selects it input, while a `fail` expr\
   ession selects nothing\n\n                id : S => S           fail : S => _|_\n\nT\
   hese are identities for the seq and alt operators.\n\n                id = (seq)  \
  \          fail = (alt)\n\nOne other pattern is so common that it is part of the la\
   nguage itself.\n\n                (try C) = (alt C id)\n\nRecords\n=======\n\nThe `reco\
   rd` syntax is used to rewrite record sexps of the form\n((field1 value1) ... (fie\
   ldn value_n)).  With `record`, one specifies for\neach field name how to change t\
   he value of the field with that name, and\noptionally what to do with fields not \
   explicitly mentioned.  For example:\n\n    (record (a1 delete) (a2 (const 13)) (a3\
  \ (rewrite $X ($X $X))))\n    :  ((a1 v1) (a2 v2) (a3 v3))\n    => (        (a2 13)\
  \ (a3 (v3 v3)))\n\nBy default, a record rewrite only succeeds if all of the fields \
   appear in the\ninput.  Also by default, fields that appear in the input that are \
   not mentioned\nin the record are preserved.\n\nOne can allow the rewrite to succeed\
  \ even if a field is missing by adding an\nattribute.\n\n    (record (f1            \
   delete)) : ((f2 v2)) => _|_\n    (record (f1 (optional) delete)) : ((f2 v2)) => (\
   (f2 v2))\n\nFields that are in the record but not in the input are treated as if t\
   heir\nvalue is ().  This makes it possible to add fields.  For example:\n\n    (rec\
   ord (a1 (optional) id         )) : () -> ((a1 ()))\n    (record (a1 (optional) (c\
   onst foo))) : () -> ((a1 foo))\n\nOne can use the special `_` as a field name to e\
   xplicitly specify what to\ndo with fields in the input that aren't mentioned in t\
   he record.\n\n    (record (a1 (const 13)) (_ id))    # the default, same as not us\
   ing _\n    :   ((a1 v1) (a2 v2))\n    ==> ((a1 13) (a2 v2))\n\n    (record (a1 id) (\
   _ delete))        # delete unmentioned fields\n    :   ((a1 v1) (a2 v2) (a3 v3))\n\
  \    ==> ((a1 v1))\n\n    (record (a1 id) (_ fail))         # fail if there are unm\
   entioned fields\n    :   ((a1 v1) (a2 v2)\n    ==> _|_\n\nOne can optionally specify\
  \ a new field name, which will cause the field\nname to be changed.\n\n    (record (\
   a1 ((rename a2)) id)) : ((a1 13)) -> ((a2 13))\n\nThere are a couple of well-forme\
   dness rules on record syntax.\n\n    well-formedness rule                         \
  \ non-conforming program\n    ----------------------------------------------------\
   -------------------------\n    a field name can occur at most once           (rec\
   ord (foo id) (foo delete))\n    if there an _ field, it must occur last       (re\
   cord (_ delete) (foo id))\n\nTraversals\n==========\n\nThe expression `(children C)` \
   transforms all the immediate\nsub-expressions of a s-expression according to C an\
   d returns the result.\n\n          (children (rewrite foo bar)) : (foo foo) => (ba\
   r bar)\n\nIf C fails on any of the children, the failure propagates outward.\n\n    \
  \      (children (rewrite foo bar)) : (foo wow) => _|_\n\nThis is easily overcome, \
   if desired\n\n    (children (try (rewrite foo bar))) : (foo wow) => (bar wow)\n\nIf \
   the input is atomic (i.e., there are no children), then C is trivially\nsuccessfu\
   l on *all* the children.\n\n          (children (rewrite foo bar)) : wow => wow\n\nT\
   op-down and bottom-up iterated traversal strategies are defined recursively\nin t\
   erms of children expressions.\n\n    (topdown C) = (seq C (children (topdown C)))\n\
   \n    (bottomup C) = (seq (children (bottomup C)) C)\n\nFor example:\n\n     (topdown\
  \ (try (rewrite a b))) : (a (c a)) => (b (c b))\n    (bottomup (try (rewrite a b))\
   ) : (a (c a)) => (b (c b))\n\nThe difference between topdown and bottomup can be s\
   een here:\n\n    if\n\n    C = (try (rewrite (not (and $A $B)) (or (not $A) (not $B)\
   )))\n\n    then\n\n    (topdown C)\n      : (not (and a (and b c))) => (or (not a) (o\
   r (not b) (not c)))\n\n    but\n\n    (bottomup C)\n      : (not (and a (and b c))) =\
   > (or (not a) (not (and b c)))\n\nNote that one must take care when using topdown \
   to avoid infinite loops!\n\n    (topdown (rewrite a (a a))) : a => ...      (never\
  \ returns!)\n\nDeletion\n========\n\n`delete` allows one to delete a sexp from its con\
   taining sexp.  Semantically,\nit is essentially a special kind of return value th\
   at is recognized by other\nconstructs, like `children`, and causes them to delete\
  \ the component sexp.\n\n    delete : foo => _|_\n     (children delete) : (foo bar)\
  \ => ()\n     (children (alt (rewrite foo 13) delete)) : (foo bar) => (13)\n\n`delet\
   e` satisfies the following equalities\n\n    (seq delete C) == delete\n    (alt del\
   ete C) == delete\n\nMiscellaneous\n=============\n\n`lowercase` does exactly what you\
  \ would think it does.\n\n    lowercase : Word        => word\n    lowercase : UPPER\
   CASE   => uppercase\n    lowercase : CamelCase   => camelcase\n    lowercase : (A \
   (B C) D) => (a (b c) d)\n    lowercase : 1234        => 1234\n\n`concat` concatenat\
   es all the atoms of the input expression into a single\natom.\n\n    concat : Word \
  \         => Word\n    concat : (' \"A B\" ') => \"'A B'\"\n    concat : (A (B C) D)   \
   => ABCD\n\nSub-queries\n===========\n\nFinally, we have a way to call the query langu\
   age from the change\nlanguage.  The semantics of `(query Q)` are to gather up all\
  \ the\ns-expressions output by running the query Q against the input and\ngathering\
  \ them all up into a single list.\n\n    (query Q) : S => (X1 ... Xn)    if Q : S =\
   > {X1, ... , Xn}\n\n(see the query subcommand's internal documentation for informa\
   tion on\nquery semantics).\n\nNote that this operation always succeeds (never resul\
   ts in _|_).\n"
;;

let query_by_example_dot_md =
  "# Sexp query by example\n\nSexp query is a command-line tool for getting data out \
   of S-expressions. Really it's a\nmini programming language, and if you want to us\
   e it effectively you'll want to see lots of\nexamples. That's what this README is\
  \ for.\n\nSee also 'sexp pat-query' for a slightly simpler regular-expression-like \
   language that is \nless powerful but can accomplish almost all of the same common\
  \ tasks.\n\nTable of contents\n=================\n\n- Basic sexp query commands and ho\
   w they work\n    - Field\n    - Index\n    - Equals and Test\n    - Pipe\n    - Each\n\
  \    - Smash\n    - Regex\n    - Cat and Wrap\n- \"sexp select\" and \"sexp multi-selec\
   t\"\n\nBasic sexp query commands and how they work\n================================\
   ==============\n\nThe basic building blocks of a sexp query are the commands `fiel\
   d`, `index`, `equals`,\n`test`, `pipe`, `each`, `smash`, `regex`, `cat`, and `wra\
   p`. (There are a few other\ncommands but they're less important.) We'll explain e\
   ach of these in turn before getting\ninto more complicated examples.\n\nField\n-----\
   \n\n`field` gets the value from a `(key value)` pair. Suppose you've got a record \
   with two\nfields, `jane_symbol` and `bloomberg`. Well, calling `(field jane_symbo\
   l)` gets you the\nvalue of that field:\n\n```sh\n  $ echo '((jane_symbol \"AAPL US\") \
   \\\\\n           (bloomberg \"AAPL UW Equity\"))' | sexp query '(field jane_symbol)'\n\
  \  # => \"AAPL US\"\n```\n\nIndex\n-----\n\n`index` is a little dumber, in that it just g\
   ives you the nth element in a list, indexed\nfrom 0:\n\n```sh\n  $ echo \"(one two th\
   ree four five)\" | sexp query '(index 2)'\n  # => three\n```\n\nIf you've got a singl\
   e-element list, like `(one)`, calling `(index 0)` on it is a nifty\ntrick for rem\
   oving the parens.\n\nEquals and Test\n---------------\n\n`equals` and `test` are ofte\
   n used together, because `equals` alone is rarely what you\nwant. It just returns\
  \ a value if the value is equal to some string, and nothing otherwise.\nSuppose we\
  \ had a little corpdir expressed as an S-expression (or really, a series of\nexpre\
   ssions, each separated by a blank line):\n\n```ocaml\n  ;; ./corpdir.sexp\n\n  ((name\
  \ ((first Bill) (last Nye)))\n   (role \"Science guy\")\n   (start_date ((year 2012) \
   (month 3) (day 4))))\n\n  ((name ((first Zadie) (last Smith)))\n   (role \"Author\")\n\
  \   (start_date ((year 2016) (month 10) (day 21))))\n```\n\nNow we can run some `equ\
   als` checks against it. Notice below how the `equals` expression\nfollows the `(f\
   ield role)` expression; that's because it's operating on the output of\n`(field r\
   ole)`, as if piping the results of the first command into the second. (We'll see\
   \nhow this works under the hood once we get to the `pipe` command.)\n\n```sh\n  $ ca\
   t corpdir.sexp | sexp query '(field role) (equals \"Author\")'\n  # => Author\n\n  $ \
   cat corpdir.sexp | sexp query '(field role) (equals \"Science guy\")'\n  # => \"Scie\
   nce guy\"\n\n  $ cat corpdir.sexp | sexp query '(field role) (equals \"Foo\")'\n  # =>\
   \n```\n\nYou can see why this isn't very useful: in general when we test a record f\
   or something, we\nwant to *do* something with that record. But here we just retur\
   n the value we're testing\nagainst. This is where `test` comes in:\n\n```sh\n  $ cat\
  \ corpdir.sexp | sexp query '(test (field role) (equals \"Author\"))'\n  # => ((name\
  \ ((first Zadie) (last Smith))) (role Author)\n  # => (start_date ((year 2016) (mo\
   nth 10) (day 21))))\n\n  $ cat corpdir.sexp | sexp query \\\\\n      '(test (field ro\
   le) (equals \"Author\")) (field start_date)'\n  # => ((year 2016) (month 10) (day 2\
   1))\n```\n\n`test` wraps around a condition, like `(equals 'foo')`, and if the cond\
   ition passes, it\nreturns the entire S-expression satisfying the condition. You c\
   an then pass this to the\nright -- here, to the `(field start_date)` operator -- \
   to get at some field within that\nreturned value. A lot of sexp queries work this\
  \ way: first you filter for records\nsatisfying a condition, and then you dig in t\
   o those looking for specific data.\n\nPipe\n----\n\n`pipe` gets its name from the Uni\
   x command-line `|` that passes output from one program to\nanother. It's a way of\
  \ chaining together a sequence of commands, like a\n`Sequence.concat_map` for sexp\
  \ queries.\n\nIn many cases you don't actually have to write the pipe, because it's\
  \ already there\nimplicitly. For instance this query:\n\n```sh\n  $ cat corpdir.sexp \
   | sexp query '(field start_date) (field day)'\n  # => 4\n  # => 21\n```\n\ntakes the \
   result of the first command, `(field start_date)`, and implicitly pipes it to\nth\
   e next, `(field day)`. It's as if there's a literal `|` pipe character between t\
   he two\nstatements. Writing it with the actual pipe command gives:\n\n```sh\n  $ cat\
  \ corpdir.sexp | sexp query '(pipe (field start_date) (field day))'\n  # => 4\n  # \
   => 21\n```\n\nOne pipe command can take an arbitrary number of sub-statements, not \
   just 2, as in the\nfollowing example:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(\
   pipe (field start_date) (field day) (equals 4))'\n  # => 4\n```\n\nIt's worth asking\
   : if there's syntactic sugar to get rid of these explicit pipes, do you\never rea\
   lly need the `pipe` command? In fact you do. While some commands, like `test`, c\
   an\ntake a series of substatements without requiring a pipe, others, like `cat` a\
   nd `unquote`,\nwhich we'll see later, require it. So you'll see `pipe` all over, \
   usually in places where\nyou have a complex sub-query, i.e., a query that involve\
   s more than a single `(field ...)`\ncommand.\n\nEach\n----\n\n`each` is pretty simple:\
  \ it takes each element in a list and passes them one by one to\nanother expressio\
   n. At the top level, you can use it like:\n\n```sh\n  <list> each <expression>\n```\n\
   \nas in the following example:\n\n```sh\n  $ cat corpdir.sexp | sexp query \\\\\n      \
   '(test (field role) (equals \"Science guy\")) (field name) each (index 1)'\n  # => \
   Bill\n  # => Nye\n```\n\nIt's worth dwelling a bit on what's happening here. On the \
   left-hand side of the `each`,\nyou have an expression that returns the `name` fie\
   ld of the record where the role field\nhas the value \"Science guy\". So what you'r\
   e passing to the `each` is the list:\n\n```sh\n  ((first Bill) (last Nye))\n```\n\nwhi\
   ch has two elements. (Notice how our little command-line program returns two lin\
   es.)\nThen, the right-hand side of the `each` is just an expression that operates\
  \ on each\nelement of the list, so on `(first Bill)` and `(last Nye)` in turn. `(i\
   ndex 1)` returns\nthe second element of whatever it's passed, which is how we end\
  \ up with \"Bill\" and \"Nye\".\n\n(`each` appears to be an infix operator because of t\
   he implicit pipe\nat the top level. But if you were to use it inside of a `test`,\
  \ for\nexample, as in `(test (pipe (field hosts) each)`, you must explicitly\npipe \
   the output of `field` to the `each`.)\n\nSmash\n-----\n\nsmaaaaasssshhhh!!!! This one\
  \ has the coolest name, and also, in a way, the coolest\nbehavior: It takes an S-e\
   xpression and returns every sub-expression of it. Then, like\n`each`, it lets you\
  \ apply a command to every one of those sub-expressions. So it also uses\nthat inf\
   ix-style `<expression> smash <expression>` syntax. But let's see what it looks\nl\
   ike when we operate on the whole _corpdir.sexp_ file, without actually doing any\
   thing\nwith the smashed contents:\n\n```sh\n  $ cat corpdir.sexp | sexp query 'smash\
   '\n  # => ((name ((first Bill) (last Nye))) (role \"Science guy\")\n  # =>  (start_d\
   ate ((year 2012) (month 3) (day 4))))\n  # => (name ((first Bill) (last Nye)))\n  \
   # => name\n  # => ((first Bill) (last Nye))\n  # => (first Bill)\n  # => first\n  # \
   => Bill\n  # => (last Nye)\n  # => last\n  # => Nye\n  # => (role \"Science guy\")\n  #\
  \ => role\n  # => \"Science guy\"\n  # => (start_date ((year 2012) (month 3) (day 4))\
   )\n  # => start_date\n  # => ((year 2012) (month 3) (day 4))\n  # => (year 2012)\n  \
   # => year\n  # => 2012\n  # => (month 3)\n  # => month\n  # => 3\n  # => (day 4)\n  # \
   => day\n  # => 4\n  #\n  # => ((name ((first Zadie) (last Smith))) (role Author)\n  \
   # =>  (start_date ((year 2016) (month 10) (day 21))))\n  # => (name ((first Zadie\
   ) (last Smith)))\n  # => name\n  # => ((first Zadie) (last Smith))\n  # => (first Z\
   adie)\n  # => first\n  # => Zadie\n  # => (last Smith)\n  # => last\n  # => Smith\n  #\
  \ => (role Author)\n  # => role\n  # => Author\n  # => (start_date ((year 2016) (mon\
   th 10) (day 21)))\n  # => start_date\n  # => ((year 2016) (month 10) (day 21))\n  #\
  \ => (year 2016)\n  # => year\n  # => 2016\n  # => (month 10)\n  # => month\n  # => 10\
   \n  # => (day 21)\n  # => day\n  # => 21\n```\n\nWhat's going on here? Well, since we \
   passed the whole file to `smash`, rather than just a\nsingle record, we're gettin\
   g the smashed contents of each of our two records in turn (one\nfor Bill Nye and \
   one for Zadie Smith). For each of these, the command is coughing up every\nsub-ex\
   pression of the original record. You can think of it as taking anything of the f\
   orm\n`<left> <right>` and printing `<left> <right>`, `<left>`, and `<right>`. Sin\
   ce\nS-expressions can be deeply nested, this can end up printing a lot of stuff.\n\
   \nSmashing is useful when you don't want to do a million chained tests in order t\
   o get to\nsome record nested deep in an S-expression. For instance, suppose our d\
   ay records were\nburied in a lot of other stuff, like so:\n\n```ocaml\n  ;; ./corpdi\
   r.sexp\n\n  ((name ((first Bill) (last Nye)))\n   (role \"Science guy\")\n   (start_da\
   te ((year 2012) (month 3) (period ((unit ((kind ((sol 400) (day 4))))))))))\n\n  (\
   (name ((first Zadie) (last Smith)))\n   (role \"Author\")\n   (start_date ((year 201\
   6) (month 10) (period ((unit ((kind ((sol 2100) (day 21))))))))))\n```\n\nIf you kn\
   ew you wanted to get at those `(day <num>)` records, you could write something\nl\
   ike:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(field start_date) (field period)\
  \ (field unit) \\\\\n                                     (field kind) each (test (i\
   ndex 0) (equals day))'\n  # => (day 4)\n  # => (day 21)\n```\n\nor... you could just \
   smash the input and filter on the field name:\n\n```sh\n  $ cat corpdir.sexp | sexp\
  \ query 'smash (test (index 0) (equals day))'\n  # => (day 4)\n  # => (day 21)\n```\n\
   \nKeep in mind that using `smash` isn't free: there's a tradeoff between being co\
   ncise and\nbeing precise when deciding whether to use it. That is, while it may b\
   e powerful for\nfinding deeply nested things, that's only because you've given up\
  \ some control over where\nthe thing is to be found. In a way, `smash` is the sexp\
  \ query analog of `.*` in regular\nexpressions. It should be used with caution.\n\nR\
   egex\n-----\n\n`regex` is like `equals`, except that instead of taking a simple str\
   ing argument, it takes\na regular expression, so that you can do slightly more ve\
   rsatile searching. Let's say we\nhad a new hire in our corpdir:\n\n```ocaml\n  ;; ./\
   corpdir.sexp\n\n  ((name ((first Bill) (last Nye)))\n  (role \"Science guy\")\n  (star\
   t_date ((year 2012) (month 3) (day 4))))\n\n  ((name ((first Zadie) (last Smith)))\
   \n  (role \"Author\")\n  (start_date ((year 2016) (month 10) (day 21))))\n\n  ((name (\
   (first David) (last Lynch)))\n  (role \"Auteur\")\n  (start_date ((year 2017) (month\
  \ 5) (day 20))))\n```\n\nIf we then wanted to get the name records of everyone whose\
  \ role starts with \"Au\", we\ncould use regex to do it:\n\n```sh\n  $ cat corpdir.sexp\
  \ | sexp query '(test (field role) (regex \"^Au\")) (field name)'\n  # => ((first Za\
   die) (last Smith))\n  # => ((first David) (last Lynch))\n```\n\nBy default, `regex` \
   will return the entire string if there's a match, and nothing if not;\nbut if you\
  \ use a capture group, as in the following example, it'll return the capture\ngrou\
   p's contents instead. (If you supply multiple capture groups it'll return the re\
   sult\nof the first one.) For instance:\n\n```sh\n  $ cat corpdir.sexp | sexp query '\
   (field role) (regex \"^Au(.*)\")'\n  # => thor\n  # => teur\n```\n\nCat and Wrap\n------\
   ------\n\n`cat` is how you run multiple commands on a single S-expression at one t\
   ime,\ncon-`cat`-enating the results. Where `pipe` is a way of combining sub-queri\
   es in series,\n`cat` combines them in parallel. So for example:\n\n```sh\n  $ cat co\
   rpdir.sexp | sexp query '(cat (field name) (field role))'\n  # => ((first Bill) (\
   last Nye))\n  # => \"Science guy\"\n  # => ((first Zadie) (last Smith))\n  # => Autho\
   r\n  # => ((first David) (last Lynch))\n  # => Auteur\n```\n\nNotice how for each rec\
   ord we've fetched both the name and the role. But also notice how\nthe results ar\
   en't wrapped up into a single S-expression. That's where `wrap` comes in --\nit's\
  \ a command that simply takes some stuff and wraps it in parens, and it's frequen\
   tly\nused together with `cat`:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(wrap (c\
   at (field name) (field role)))'\n  # => (((first Bill) (last Nye)) \"Science guy\")\
   \n  # => (((first Zadie) (last Smith)) Author)\n  # => (((first David) (last Lynch\
   )) Auteur)\n```\n\nNow the results of our multiple queries are nicely wrapped up in\
   to a single S-expression\nper record.\n\n`sexp select` and `sexp multi-select`\n====\
   =================================\n\nA lot of the time, what would be a fairly com\
   plicated sexp query is more easily expressed\nas a sexp multi-select. Suppose you\
  \ wanted to pull out the actual day, month, and year of\neach person in our little\
  \ corpdir. You could do it using sexp query:\n\n```sh\n  $ cat corpdir.sexp | sexp q\
   uery '(field start_date) \\\\\n                                     (wrap (cat (fie\
   ld day) (field month) (field year)))'\n  # => (4 3 2012)\n  # => (21 10 2016)\n  # \
   => (20 5 1999)\n```\n\nBut it's actually much easier when expressed as a multi-sele\
   ct:\n\n```sh\n  $ cat corpdir.sexp | sexp multi-select day month year\n  # => (4 3 2\
   012)\n  # => (21 10 2016)\n  # => (20 5 1999)\n```\n\nThis multi-select does a kind o\
   f `smash`, `cat`, and `wrap` on the fields that you pass to\nit. Notice that you \
   don't even need to quote your field names!\n\nBecause it's more or less doing a sm\
   ash, the same caveat applies: multi-select is a\nconcise way to find things, but \
   at the expense of being somewhat imprecise about where\nyou're looking.\n"
;;

let query_semantics_dot_md =
  "# Sexp query formal semantics\n\nSee also the get and select and pat-query subcomm\
   ands.\n\nThis document explains the semantics of query expressions, whose\nsyntax i\
   s as follows:\n\n    E ::=\n        -- selection -------------\n        | (index NUM\
   )\n        | (field STRING)\n        | each\n        | smash\n        -- composition\
  \ -----------\n        | (pipe E E ...)\n        | (cat E E ...)\n        | this\n   \
  \     | none\n        -- tests/conditionals ----\n        | atomic\n        | (varia\
   nt TAG NUM)\n        | (equals SEXP ...)\n        | (regex R)\n        | (test E E \
   ...)\n        | (not E)\n        | (and E E ...)\n        | (or E E ...)\n        | \
   (if E E E)\n        | (branch E E E)\n        -- formatting ------------\n        |\
  \ (wrap E)\n        | (quote T[0])\n        -- transformations -------\n        | (c\
   hange C)\n        | restructure\n\nThe meaning of an expression is a function from \
   an input s-expression to a\n(possibly empty) sequence of output s-expressions.  T\
   he toplevel command is\napplied to each expression in the input.  We will look at\
  \ five categories\nof expressions: selection, composition, conditionals, formattin\
   g, and\ntransformation.\n\nSelection\n---------\n\nA `(index N)` expression picks out \
   the Nth element of a list s-expression.\nIt succeeds if N is a proper index (i.e.\
   , 0 <= N < list length) and fails\notherwise.  Upon success, it returns the singl\
   e-element sequence containing\nthe selected sub-expressions.  Upon failure, it re\
   turns an empty list.\n\n    (index 2) : (one two three four) => {three}\n    (index\
  \ 8) : (one two three four) => {}\n\nA `(field F)` expression is like `(index N)` e\
   xcept that it projects\nthe field named F out of a record.  If more than one fiel\
   d of that\nname exists, they will all be selected.\n\n    (field foo) : ((bar 1) (f\
   oo 2) (baz 3))         => {2}\n    (field foo) : ((bar 1) (foo 2) (baz 3) (foo 4)\
   ) => {2, 4}\n    (field wow) : ((bar 1) (foo 2) (baz 3))         => {}\n\nAn `each`\
  \ expression selects every element of a list.\n\n    each : (one two three four) =>\
  \ {one, two, three, four}\n    each : ()    => {}\n    each : hello => {}\n\nAn `smas\
   h` expression selects every sub-expression of an s-expression\n\n    smash : (a (b\
  \ c) (d (e f))) => { (a (b c) (d (e f))),\n                                      a\
   , (b c), (d (e f)),\n                                          b, c,  d, (e f),\n \
  \                                                    e, f }\n\nComposition\n--------\
   ---\n\nThe two basic composition operators are `pipe` and `cat`.  These two\noperat\
   ors do sequential and parallel composition, respectively.  The\ndenotation of\n\n  \
  \                  (pipe E1 E2 ... En)\n\nis a \"fanned out\" composition of the deno\
   tations D = [E1] and\nD' = [`(pipe E2 ... En)`] in which D' is called on every s-\
   expression\nin the output sequence of D and all the resulting sequences of\ns-expr\
   essions are concatenated together into one big sequence.\n\n            (pipe E) =\
  \ E\n            (pipe E1 E2 ... En) : S => X1 U ... U Xm\n\n            where   E1 \
   : S => {S1, ..., Sm}\n              and   (pipe E2 ... En) : Si => Xi   for i in \
   {1..m}\n\nIn particular, this means that if D returns an empty sequence, then\nD' w\
   ill never be called.  The denotation of\n\n                    (cat E1 E2 ... En)\n\
   \nis simply a parallel execution of each Ei on the input X in which all\n\n        \
  \    (cat E1 ... En) : S => X1 U ... U Xn\n\n            where   Ei : S => Xi    fo\
   r i in {i .. n}\n\nThere are two trivial expressions, `this` and `none`.  A this e\
   xpression\nsimply selects it input, while a `none` expression selects nothing\n\n  \
  \              this : S => {S}         none : S => {}\n\nThese are identities for t\
   he pipe and cat operators.\n\n                this = (pipe)          none = (cat)\n\
   \n(For monad fans -- pipe is Kleisli composition and `this` is Kleisli\nidentity f\
   or the list monad.)\n\n\nConditionals\n------------\n\nFor the purpose of conditional \
   execution, we treat an expression returning\na non-empty sequence as true, and an\
  \ expression returning an empty sequence\nas false.  With this in mind, one may th\
   ink of `this` and `none` as the\nconstants true and false, respectively.\n\nA `atom\
   ic` expression tests for atomic s-expressions.\n\n        atomic : foo       => {f\
   oo}\n        atomic : (foo bar) => {}\n\nThe most basic non-trivial condition is eq\
   uality.  The expression\n`(equals S)` selects its input in the case that the (ret\
   urns true) and\nfails (returns false) otherwise.\n\n        (equals S) : S' => {S'}\
  \    if S = S'\n        (equals S) : S' => {}      otherwise\n\nThere is also a N-ar\
   y version of equals that is expands to a disjunction\n\n        (equals S1 S2 ...)\
  \ = (or (equals S1) (equals S2) ...)\n\nA `(regex R)` expression tests for the s-ex\
   presion to be an ATOM that\nmatches R.  The truth value will be the first capturi\
   ng group of the\nregular expression or the whole atom if regular expression has n\
   o\ncapturing groups.\n\nA `(variant F N)` expression tests for the s-expression for\
   m of an element\nof a variant type with constructor F applied to N arguments.  Yo\
   u can leave N off, in\nwhich case it will match a variant with any number of argu\
   ments.\n\n    (variant foo 5) : (foo 1 2 3 4 5) => {(foo 1 2 3 4 5)}\n    (variant \
   foo 3) : (foo 1 2 3 4 5) => {}\n    (variant foo 8) : (foo 1 2 3 4 5) => {}\n    (\
   variant bar 5) : (foo 1 2 3 4 5) => {}\n    (variant foo 0) : foo => {foo}\n    (v\
   ariant foo 0) : (foo) => {(foo)}\n    (variant foo)   : (foo 1 2 3 4 5) => {(foo \
   1 2 3 4 5)}\n    (variant foo)   : foo => {foo}\n    (variant foo)   : (foo) => {(\
   foo)}\n\nA `(test E)` expression proceeds by evaluating E on the current s-express\
   ion\nS and selecting S only in the case that E succeeds.\n\n        (test E) : S =>\
  \ {}    if X empty      where  E : S => X\n        (test E) : S => {S}   otherwise\
   \n\nWe also provide `(test E1 E2 ...)` as syntactic sugar for the common\nidiom `(t\
   est (pipe E1 E2 ...))`.\n\nA `(not E)` expression proceeds by evaluating E on the \
   current s-expression\nS and selecting S only in the case that E fails.\n\n        (\
   not E) : S => {S}    if X empty      where  E : S => X\n        (not E) : S => {}\
  \     otherwise\n\nNote that X is is discarded from the output of both (test E) and\
  \ (not E).\nFor this reason, these two operators are useful for \"looking down\" int\
   o an\ns-expression while remembering your place at some point above where you wil\
   l\nreturn later.\n\nAn `(and E1 ... En)` expression does short-circuit evaluation b\
   ased on whether\nor not E1 succeeds.  Upon success, it returns the results of En.\
   \n\n    (and) = this\n\n    (and E) = E\n\n    (and E1 E2 ... En) : S => {}        if \
  \ E1 : S => {}\n    (and E1 E2 ... En) : S => Y         if  E1 : S => X  (nonempty\
   )\n                                        and (and E2 ... En) : S => Y\n\nAn `(or \
   E1 ... En)` expression does short-circuit evaluation based on whether\nor not E1 \
   succeeds.  It returns the results of the first Ei that succeeds.\n\n    (or) = non\
   e\n\n    (or E) = E\n\n    (or E1 E2 ... En) : S => X         if  E1 : S => X  (none\
   mpty)\n    (or E1 E2 ... En) : S => Y         if  E1 : S => {}\n                  \
  \                     and (or E2 ... En) : S => Y\n\nAn `(if E1 E2 E3)` expression \
   does conditional execution of E2 or E3 based\non whether or not E1 succeeds.\n\n   \
  \ (if E1 E2 E3) : S => X2      if E1 : S => X (non-empty) and\n    (if E1 E2 E3) :\
  \ S => X3      if E1 : S => {}\n\n       where   Ei : Si => Xi    for i in {2,3}\n\nA\
   n `(branch E1 E2 E3)` expression does conditional execution like `if`, but\nalso \
   pipes the output of the condition into the `then' branch.\n\n    (branch E1 E2 E3)\
  \ : S => X1 U ... U Xn\n\n                                    if E1 : S => {S1, ...\
  \ , Sn} (non-empty)\n                                    and E2 : Si => Xi  for i \
   in {1..n}\n\n    (branch E1 E2 E3) : S => X3\n                                    i\
   f E1 : S => {}\n                                    and E3 : S3 => X3\n\nThe follow\
   ing equations relating the behavior of `if` and `branch` are true:\n\n    (if E1 E\
   2 E3) = (branch (test E1) E2 E3)\n\n    (branch E1 E2 E3) = (if E1 (pipe E1 E2) E3\
   )\n\n\nFormatting\n----------\n\nUsing the commands so far, one may only output sub-ex\
   pressions of the input\nexpression itself.  However, we may also want to impose a\
   dditional structure\non the output.\n\nA `(wrap E)` expression runs E and gathers u\
   p the resulting sequence into a\nsingle list s-expression that becomes the (singl\
   e) overall result.\n\n    (wrap E) : S => {(S1 ... S2)}  where  E : S => {S1, ... \
   , S2}\n\nNote that the final output sequence has exactly one element that is a lis\
   t.\n\nA `(quote S)` expression adds the provided s-expression to the manifest.\n\n  \
  \  (quote S') : S => {S'}\n\nNote that S is discarded here.  For this reason, quote\
  \ is often used in\nconjunction with a parallel composition operator like `cat`, `\
   and`, or `or`.\n\n    (quote (a b c))              : (1 2 3) => {(a b c)}\n    (quo\
   te (a (unquote each) c)) : (1 2 3) => {(a 1 c), (a 2 c), (a 3 c)}\n    (quote (a \
   (splice each) c))  : (1 2 3) => {(a 1 2 3 c)}\n\n    (quote (a (splice each) c (un\
   quote each)))\n        : (1 2 3) => {(a 1 2 3 c 1), (a 1 2 3 c 2), (a 1 2 3 c 3)}\
   \n\nMultiple unquotes in a single quoted template yield a cartesian product.\n\n    \
   (quote (a (unquote (pipe (index 0) each))\n            b (unquote (pipe (index 1)\
  \ each))))\n        : ((1 2 3) (x y z)) => {(a 1 b x), (a 1 b y), (a 1 b z),\n     \
  \                           (a 2 b x), (a 2 b y), (a 2 b z),\n                    \
  \            (a 3 b x), (a 3 b y), (a 3 b z)}\n\nFurthermore, nested quotes increas\
   e the \"degree\" of quotation.  Splice and\nunquote only have their effect at the d\
   egree zero.  This feature is intended\nto facilitate sexpquery expressions that m\
   anipulate other sexpquery expressions.\n\n--- grammar for sexpquery templates ---\n\
   \n          T[0] ::= ATOM               T[n+1] ::= ATOM\n                 | (T[0] \
   ... T[0])             | (T[n+1] ... T[n+1])\n                 | (quote T[1])     \
  \           | (quote T[n+2])\n                 | (unquote E)                 | (un\
   quote T[n])\n                 | (splice E)                  | (splice T[n])\n\nTran\
   sformations\n---------------\n\nFinally, we have a way to call the change language \
   from the query\nlanguage.  The semantics of (change C) are to return the transfor\
   med\nexpression as a singleton sequence and propagate failure.\n\n    (change C) : \
   S => {S'}    if C : S => S'\n    (change C) : S => {}      if C : S => _|_\n\n(see \
   the change subcommand's internal documentation for information on\nchange semanti\
   cs).\n\nSometimes the contents of an atom will be the string representation of\na s\
   equence of sexps. `restructure` will do this interpretation for you:\n\n    restru\
   cture : \"A (B C) D\" => {A (B C) D}\n"
;;

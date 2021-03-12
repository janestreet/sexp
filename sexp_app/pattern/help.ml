let pat_query_readme =
  {|
=== Pattern syntax summary ===
For a longer explanation with examples, run 'sexp pat-query' with '-examples'.

For a more precise specification of the semantics and the underlying type that corresponds
to the grammar, see:
https://ocaml.janestreet.com/ocaml-core/latest/doc/sexp_app_pattern/Sexp_app_pattern/Query/index.html

.             : Matches any sexp
abc           : Matches atom 'abc'
/regex/       : Matches atom via regular expression
foo*          : Matches foo zero or more times
foo+          : Matches foo one or more times
foo?          : Matches foo zero or one times
( )           : Matches a sexp list as a list, enforcing order and exact length
{ }           : Matches a sexp list as a set, not enforcing order AND allowing extra elements.
[ ]           : Limit the scope of things (see examples below)

.. foo        : Searches for foo anywhere within any subexpression of the sexp.
              : This is a lot like sexp query's "smash".

%.            : Simple capture
%(a %. c)     : Simple captures can also be put on other subpatterns, or nested.
%0 %1         : Numbered captures
%abc %def     : Named captures
%abc=(a %. c) : Named or numbered captures can also be used on subpatterns or nested.

a bc | de f   : Matches 'a bc' OR 'de f'.
a [bc | de] f : Matches 'a', then 'bc' OR 'de', then 'f'.
a b & c d     : Matches any sexp that matches both 'a b' AND 'c d'.

[.. a & .. b] : Matches any sexp that has 'a' anywhere within, AND has 'b' anywhere within.

!foo          : Stop matching after the first match that foo finds.
{ foo? }      : Matches foo to elements of the sexp list but accepts the lists where none match foo
{ !foo }      : Stops matching foo once it matches for some element of the list

foo*+         : Matches foo zero or more times, longer matches first (greedy)
foo++         : Matches foo one or more times, longer matches first (greedy)
foo?+         : Matches foo zero or one times, longer matches first (greedy)
|}
;;

let pat_query_examples =
  {|
EXAMPLES OF HOW TO USE:

Write a sexp-like expression for the pattern you're trying to match and use "%." to
capture a value you want.

Example pattern: (a b %.)
Effect:
(a b c)      -> c
(a b ())     -> ()
(a b (c d))  -> (c d)
(a b c d)    ->  <<no match>>
(a b)        ->  <<no match>>

To capture multiple values into a list, you can use numbered captures (zero-indexed).
Example pattern: (%0 b %1)
Effect:
(a b c) -> (a c)

Or you can capture them into records by using named captures.
Example pattern: (%foo b %bar)
Effect:
(a b c) -> ((foo a) (bar c))

You can also use -format to specify an explicit output format, like:
Example pattern: (%foo b %bar), with -format (%foo (abc %bar))
Effect:
(a b c) -> (a (abc c))

Regular parens require an exact match in order and length, but you can use curly brackets
if you need to be robust to the order that things appear in something like a record, and
you don't care if there are other fields:
Example pattern: { (name %0) (time %1) }
Effect:
((name Alice) (time 9:00) (qty 3)) -> (Alice 9:00)
((time 10:00) (name Bob))          -> (Bob 10:00)

Most of the time, if you have a big sexp and you only want some deep piece of it,
you can use '..', which will descend deep into any subsexp and return all matches.
Example pattern:  .. (sym %.)
Effect:
(my giant sexp ... (( more stuff ... (sym foo)...)) ... (sym bar) ... )
->
foo            <---- Two separate matches
bar            <---/

'..' also works within subexpressions. For example, the following would search for a
subrecord with a field "id", and anywhere deeper in it for a field "routes", and capture
all the routes by id:

Example pattern: .. { (id %0) .. (routes { %1 }) }
Effect:
(some giant sexp... ( ((id FOO) ((... (routes (A B)))))
                      ((id BAR) ((... (routes (A E))))) ))
->
(FOO A)
(FOO B)
(BAR A)
(BAR E)

You can use '.' to match any value, and * to match something zero or more times.
For example, the following would grab the second element out of any subsexp:
Example pattern: .. (. %. .*)
Effect:
(a (b c) (d e f))
->
(b c)
c
e

You can use '?' to express that a value might or might not be there. For example,
if you have two fields that are both optional:
Example pattern: { (start (%0?)) (stop (%1?)) }
Effect:
((start (3)) (stop (4))) -> (3 4)
((start ()) (stop (4)))  -> (() 4)
((start (3)) (stop ()))  -> (3 ())
((start ()) (stop ()))   -> (() ())

Within curly braces, '?' may be used to optionally match items in the list. This is useful
for [%sexp.option] fields.

For example this pattern:

{(a %a)? (b %b)}

will match records with a [%sexp.option] field "a" and a required field "b", capturing
those fields. It will still succeed if the "a" field doesn't exist (while capturing
nothing for "%a").

Example pattern: { (foo %0) (bar %1)? (baz %2)? }
Effect:
((foo x))               -> (x () ())
((foo x)(baz z))        -> (x () z)
((baz z)(foo x)(bar y)) -> (x y z)
((baz z))               -> <<no match>>

You can use '&' for AND and '|' for OR.
Use square brackets to delimit the scope of things where needed.
Examples:

Search anywhere for a triple of a, then b OR c, then d, and capture the b OR c:
.. (a %[b | c] d)

Search separately anywhere for a field "foo" and anywhere for a field "bar", and return
the cross product of all the things they match:
.. (foo %foo) & .. (bar %bar)

You can use % on whole sexps as well. For example, to capture all records that have
the field "count" anywhere ("." matches any sexp):
.. %{(count .)}

Or to capture it in a named or numbered fashion:
.. %foo={(count .)}

That's most of the basic things you can do!

|}
;;

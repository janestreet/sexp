# Sexp query by example

Sexp query is a command-line tool for getting data out of S-expressions. Really it's a
mini programming language, and if you want to use it effectively you'll want to see lots of
examples. That's what this README is for.

See also 'sexp pat-query' for a slightly simpler regular-expression-like language that is
less powerful but can accomplish almost all of the same common tasks.

Table of contents
=================

- Basic sexp query commands and how they work
    - Field
    - Index
    - Equals and Test
    - Pipe
    - Each
    - Smash
    - Regex
    - Cat and Wrap
- "sexp select" and "sexp multi-select"

Basic sexp query commands and how they work
==============================================

The basic building blocks of a sexp query are the commands `field`, `index`, `equals`,
`test`, `pipe`, `each`, `smash`, `regex`, `cat`, and `wrap`. (There are a few other
commands but they're less important.) We'll explain each of these in turn before getting
into more complicated examples.

Field
-----

`field` gets the value from a `(key value)` pair. Suppose you've got a record with two
fields, `animal_name` and `age`. Well, calling `(field animal_name)` gets you the
value of that field:

```sh
  $ echo '((animal_name "capybara") \\
           (age 3))' | sexp query '(field animal_name)'
  # => "capybara"
```

Index
-----

`index` is a little dumber, in that it just gives you the nth element in a list, indexed
from 0:

```sh
  $ echo "(one two three four five)" | sexp query '(index 2)'
  # => three
```

If you've got a single-element list, like `(one)`, calling `(index 0)` on it is a nifty
trick for removing the parens.

`index` also supports negative indexing, allowing you to get elements relative to the end
of the list rather than to the start:

```sh
  $ echo "(one two three four five)" | sexp query '(index -1)'
  # => five
```

Equals and Test
---------------

`equals` and `test` are often used together, because `equals` alone is rarely what you
want. It just returns a value if the value is equal to some string, and nothing otherwise.
Suppose we had a little corporate directory expressed as an S-expression (or really, a
series of expressions, each separated by a blank line):

```ocaml
  ;; ./corpdir.sexp

  ((name ((first Bill) (last Nye)))
   (role "Science guy")
   (start_date ((year 2012) (month 3) (day 4))))

  ((name ((first Zadie) (last Smith)))
   (role "Author")
   (start_date ((year 2016) (month 10) (day 21))))
```

Now we can run some `equals` checks against it. Notice below how the `equals` expression
follows the `(field role)` expression; that's because it's operating on the output of
`(field role)`, as if piping the results of the first command into the second. (We'll see
how this works under the hood once we get to the `pipe` command.)

```sh
  $ cat corpdir.sexp | sexp query '(field role) (equals "Author")'
  # => Author

  $ cat corpdir.sexp | sexp query '(field role) (equals "Science guy")'
  # => "Science guy"

  $ cat corpdir.sexp | sexp query '(field role) (equals "Foo")'
  # =>
```

You can see why this isn't very useful: in general when we test a record for something, we
want to *do* something with that record. But here we just return the value we're testing
against. This is where `test` comes in:

```sh
  $ cat corpdir.sexp | sexp query '(test (field role) (equals "Author"))'
  # => ((name ((first Zadie) (last Smith))) (role Author)
  # => (start_date ((year 2016) (month 10) (day 21))))

  $ cat corpdir.sexp | sexp query \\
      '(test (field role) (equals "Author")) (field start_date)'
  # => ((year 2016) (month 10) (day 21))
```

`test` wraps around a condition, like `(equals 'foo')`, and if the condition passes, it
returns the entire S-expression satisfying the condition. You can then pass this to the
right -- here, to the `(field start_date)` operator -- to get at some field within that
returned value. A lot of sexp queries work this way: first you filter for records
satisfying a condition, and then you dig in to those looking for specific data.

Pipe
----

`pipe` gets its name from the Unix command-line `|` that passes output from one program to
another. It's a way of chaining together a sequence of commands, like a
`Sequence.concat_map` for sexp queries.

In many cases you don't actually have to write the pipe, because it's already there
implicitly. For instance this query:

```sh
  $ cat corpdir.sexp | sexp query '(field start_date) (field day)'
  # => 4
  # => 21
```

takes the result of the first command, `(field start_date)`, and implicitly pipes it to
the next, `(field day)`. It's as if there's a literal `|` pipe character between the two
statements. Writing it with the actual pipe command gives:

```sh
  $ cat corpdir.sexp | sexp query '(pipe (field start_date) (field day))'
  # => 4
  # => 21
```

One pipe command can take an arbitrary number of sub-statements, not just 2, as in the
following example:

```sh
  $ cat corpdir.sexp | sexp query '(pipe (field start_date) (field day) (equals 4))'
  # => 4
```

It's worth asking: if there's syntactic sugar to get rid of these explicit pipes, do you
ever really need the `pipe` command? In fact you do. While some commands, like `test`, can
take a series of substatements without requiring a pipe, others, like `cat` and `unquote`,
which we'll see later, require it. So you'll see `pipe` all over, usually in places where
you have a complex sub-query, i.e., a query that involves more than a single `(field ...)`
command.

Each
----

`each` is pretty simple: it takes each element in a list and passes them one by one to
another expression. At the top level, you can use it like:

```sh
  <list> each <expression>
```

as in the following example:

```sh
  $ cat corpdir.sexp | sexp query \\
      '(test (field role) (equals "Science guy")) (field name) each (index 1)'
  # => Bill
  # => Nye
```

It's worth dwelling a bit on what's happening here. On the left-hand side of the `each`,
you have an expression that returns the `name` field of the record where the role field
has the value "Science guy". So what you're passing to the `each` is the list:

```sh
  ((first Bill) (last Nye))
```

which has two elements. (Notice how our little command-line program returns two lines.)
Then, the right-hand side of the `each` is just an expression that operates on each
element of the list, so on `(first Bill)` and `(last Nye)` in turn. `(index 1)` returns
the second element of whatever it's passed, which is how we end up with "Bill" and "Nye".

(`each` appears to be an infix operator because of the implicit pipe
at the top level. But if you were to use it inside of a `test`, for
example, as in `(test (pipe (field hosts) each)`, you must explicitly
pipe the output of `field` to the `each`.)

Smash
-----

smaaaaasssshhhh!!!! This one has the coolest name, and also, in a way, the coolest
behavior: It takes an S-expression and returns every sub-expression of it. Then, like
`each`, it lets you apply a command to every one of those sub-expressions. So it also uses
that infix-style `<expression> smash <expression>` syntax. But let's see what it looks
like when we operate on the whole _corpdir.sexp_ file, without actually doing anything
with the smashed contents:

```sh
  $ cat corpdir.sexp | sexp query 'smash'
  # => ((name ((first Bill) (last Nye))) (role "Science guy")
  # =>  (start_date ((year 2012) (month 3) (day 4))))
  # => (name ((first Bill) (last Nye)))
  # => name
  # => ((first Bill) (last Nye))
  # => (first Bill)
  # => first
  # => Bill
  # => (last Nye)
  # => last
  # => Nye
  # => (role "Science guy")
  # => role
  # => "Science guy"
  # => (start_date ((year 2012) (month 3) (day 4)))
  # => start_date
  # => ((year 2012) (month 3) (day 4))
  # => (year 2012)
  # => year
  # => 2012
  # => (month 3)
  # => month
  # => 3
  # => (day 4)
  # => day
  # => 4
  #
  # => ((name ((first Zadie) (last Smith))) (role Author)
  # =>  (start_date ((year 2016) (month 10) (day 21))))
  # => (name ((first Zadie) (last Smith)))
  # => name
  # => ((first Zadie) (last Smith))
  # => (first Zadie)
  # => first
  # => Zadie
  # => (last Smith)
  # => last
  # => Smith
  # => (role Author)
  # => role
  # => Author
  # => (start_date ((year 2016) (month 10) (day 21)))
  # => start_date
  # => ((year 2016) (month 10) (day 21))
  # => (year 2016)
  # => year
  # => 2016
  # => (month 10)
  # => month
  # => 10
  # => (day 21)
  # => day
  # => 21
```

What's going on here? Well, since we passed the whole file to `smash`, rather than just a
single record, we're getting the smashed contents of each of our two records in turn (one
for Bill Nye and one for Zadie Smith). For each of these, the command is coughing up every
sub-expression of the original record. You can think of it as taking anything of the form
`<left> <right>` and printing `<left> <right>`, `<left>`, and `<right>`. Since
S-expressions can be deeply nested, this can end up printing a lot of stuff.

Smashing is useful when you don't want to do a million chained tests in order to get to
some record nested deep in an S-expression. For instance, suppose our day records were
buried in a lot of other stuff, like so:

```ocaml
  ;; ./corpdir.sexp

  ((name ((first Bill) (last Nye)))
   (role "Science guy")
   (start_date ((year 2012) (month 3) (period ((unit ((kind ((sol 400) (day 4))))))))))

  ((name ((first Zadie) (last Smith)))
   (role "Author")
   (start_date ((year 2016) (month 10) (period ((unit ((kind ((sol 2100) (day 21))))))))))
```

If you knew you wanted to get at those `(day <num>)` records, you could write something
like:

```sh
  $ cat corpdir.sexp | sexp query '(field start_date) (field period) (field unit) \\
                                     (field kind) each (test (index 0) (equals day))'
  # => (day 4)
  # => (day 21)
```

or... you could just smash the input and filter on the field name:

```sh
  $ cat corpdir.sexp | sexp query 'smash (test (index 0) (equals day))'
  # => (day 4)
  # => (day 21)
```

Keep in mind that using `smash` isn't free: there's a tradeoff between being concise and
being precise when deciding whether to use it. That is, while it may be powerful for
finding deeply nested things, that's only because you've given up some control over where
the thing is to be found. In a way, `smash` is the sexp query analog of `.*` in regular
expressions. It should be used with caution.

Regex
-----

`regex` is like `equals`, except that instead of taking a simple string argument, it takes
a regular expression, so that you can do slightly more versatile searching. Let's say we
had a new hire in our corpdir:

```ocaml
  ;; ./corpdir.sexp

  ((name ((first Bill) (last Nye)))
  (role "Science guy")
  (start_date ((year 2012) (month 3) (day 4))))

  ((name ((first Zadie) (last Smith)))
  (role "Author")
  (start_date ((year 2016) (month 10) (day 21))))

  ((name ((first David) (last Lynch)))
  (role "Auteur")
  (start_date ((year 2017) (month 5) (day 20))))
```

If we then wanted to get the name records of everyone whose role starts with "Au", we
could use regex to do it:

```sh
  $ cat corpdir.sexp | sexp query '(test (field role) (regex "^Au")) (field name)'
  # => ((first Zadie) (last Smith))
  # => ((first David) (last Lynch))
```

By default, `regex` will return the entire string if there's a match, and nothing if not;
but if you use a capture group, as in the following example, it'll return the capture
group's contents instead. (If you supply multiple capture groups it'll return the result
of the first one.) For instance:

```sh
  $ cat corpdir.sexp | sexp query '(field role) (regex "^Au(.*)")'
  # => thor
  # => teur
```

Cat and Wrap
------------

`cat` is how you run multiple commands on a single S-expression at one time,
con-`cat`-enating the results. Where `pipe` is a way of combining sub-queries in series,
`cat` combines them in parallel. So for example:

```sh
  $ cat corpdir.sexp | sexp query '(cat (field name) (field role))'
  # => ((first Bill) (last Nye))
  # => "Science guy"
  # => ((first Zadie) (last Smith))
  # => Author
  # => ((first David) (last Lynch))
  # => Auteur
```

Notice how for each record we've fetched both the name and the role. But also notice how
the results aren't wrapped up into a single S-expression. That's where `wrap` comes in --
it's a command that simply takes some stuff and wraps it in parens, and it's frequently
used together with `cat`:

```sh
  $ cat corpdir.sexp | sexp query '(wrap (cat (field name) (field role)))'
  # => (((first Bill) (last Nye)) "Science guy")
  # => (((first Zadie) (last Smith)) Author)
  # => (((first David) (last Lynch)) Auteur)
```

Now the results of our multiple queries are nicely wrapped up into a single S-expression
per record.

`sexp select` and `sexp multi-select`
=====================================

A lot of the time, what would be a fairly complicated sexp query is more easily expressed
as a sexp multi-select. Suppose you wanted to pull out the actual day, month, and year of
each person in our little corpdir. You could do it using sexp query:

```sh
  $ cat corpdir.sexp | sexp query '(field start_date) \\
                                     (wrap (cat (field day) (field month) (field year)))'
  # => (4 3 2012)
  # => (21 10 2016)
  # => (20 5 1999)
```

But it's actually much easier when expressed as a multi-select:

```sh
  $ cat corpdir.sexp | sexp multi-select day month year
  # => (4 3 2012)
  # => (21 10 2016)
  # => (20 5 1999)
```

This multi-select does a kind of `smash`, `cat`, and `wrap` on the fields that you pass to
it. Notice that you don't even need to quote your field names!

Because it's more or less doing a smash, the same caveat applies: multi-select is a
concise way to find things, but at the expense of being somewhat imprecise about where
you're looking.

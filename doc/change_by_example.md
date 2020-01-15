# Sexp change by example

Sexp change is a command-line tool for manipulating S-expressions,
useful if you want to do some data munging without having to write a
full-fledged program to parse and process your sexps.

This doc collects examples of sexp change in action. For a more
complete overview of the language, see
[this doc](./change_semantics.md),
which includes many small examples showing how to use each keyword.

Applying a rewrite rule throughout a file
-----------------------------------------

Perhaps the most common way to use `sexp change` is just to barrel
through a file attempting to apply a rewrite rule. You can accomplish
this with the following:

```
(topdown (try (rewrite LHS RHS)))
```

The `topdown` keyword ensures you cover the entire file, while `try`
ensures that if you can't apply the rewrite rule to a given expression
LHS, you leave LHS as it is.

For convenience, this functionality is exposed with the `sexp rewrite` command.

Using sexp query and sexp change together
-----------------------------------------

Here are a pair of examples making use of sexp query's `index`,
`pipe`, `quote` and `unquote` commands, and sexp change's `seq`,
`rewrite`, and `concat` commands.

Suppose your input is something like:

```
(AMZN ( ... ))
(MSFT ( ... ))
```

and you want to change it to:

```
("AMZN US" ( ... ))
("MSFT US" ( ... ))
```

Here's how you could do it using the `change` subcommand of `sexp query`:

```sh
sexp query '
(quote (
  (unquote (pipe (index 0) (change (seq (rewrite $X ($X " US")) concat))))
  (unquote (index 1))))
' <<EOF
(AMZN (foo bar))
(MSFT (foo bar))
EOF
```

Let's break down how this works. From
[the sexp query readme](query_by_example.md) you know how to use
`quote` to create an S-expression "template" that is then filled in
with `unquote` statements. Here, that template is going to look like
`(A B)`, where the `A` part is filled in by the results of:

```sh
(pipe (index 0) (change (seq (rewrite $X ($X " US")) concat)))
```

We pipe the results of `(index 0)`, i.e., something like "AMZN", to
the `change` command, which consists of a `seq`-uence of two
subcommands: `(rewrite $X ($X " US"))` takes the single S-expression
like `AMZN` (an atom) and rewrites it as `(AMZN " US")`. `concat` then
concatenates this 2-atom list into the single atom `"AMZN US"`.

Finally, the second half of the `quote` statement plops the result of
`(unquote (index 1))` into the latter half of the list, creating the
desired `(AMZN (foo bar))`.

Suppose we wanted to do this for sexps of more than two items, like
so:

```
(AMZN ( ... ) ( ... ))
(MSFT ( ... ) ( ... ) ( ... ))
```

Then you'd want to use the `query` subcommand of `sexp change`:

```sh
sexp change '
(seq
  (rewrite ($A @B) ($A (@B)))
  (query (
    quote (
      (unquote (pipe (index 0) (change (seq (rewrite $X ($X " US")) concat))))
      (unquote (index 1)))))
  (rewrite (($A (@B))) ($A @B)))
' <<EOF
(AMZN (foo bar) baz)
(MSFT (foo bar))
EOF
```

Notice how the inner part of this command is the exact same `query`
we just did. But that's step 2 of a sequence of three steps, the first
of which is to wrap the "tail" of our list in parens, via:

```sh
(rewrite ($A @B) ($A (@B)))
```

and then, after we're done with the meat of the rewrite, unwrap it
again via the inverse:

```sh
(rewrite (($A (@B))) ($A @B))
```

where the extra parens around `(($A (@B)))` are added because the
`query` subcommand of `sexp change` wraps its results in a list.

Overwriting all record values with a given key
----------------------------------------------

See the following little shell command:

```sh
function proxy-iron-config-command {
    no-args "$@"
    host=james
    sexp change </etc/iron-config.sexp \
         "(record (host (const $host)))"
}
```

This operates on a file with S-expressions of the form:

```ocaml
((host some_host_name)
 (async_rpc_port (Static 7013))
 (hgrc /path/to/hgrc)
 (serializer_pause_timeout 2m)
 (rpc_proxy_config
  ((another_host1 (p1 p2 p3))
   (another_host2 (p1 p2)))))
```

i.e., records with keys `host`, `async_rpc_port`, `hgrc`,
`serializer_pause_timeout`, and so on. By writing
`(record (host (const $host)))`, the above sexp change program just
takes whatever the value of the interpolated `$host` ends up being --
this is a shell variable, not a variable in the sexp change language
-- and jams it into the place of _every_ `host` record value. So if
`$host` were "foo", the above record would be rewritten as:

```ocaml
((host foo)
 (async_rpc_port (Static 7013))
 (hgrc /path/to/hgrc)
 (serializer_pause_timeout 2m)
 (rpc_proxy_config
  ((another_host1 (p1 p2 p3))
   (another_host2 (p1 p2)))))
```

and so on for the rest of the records in the file -- they would all
begin `(host foo)`.

Doing a bottom-up search with alt and try
-----------------------------------------

Here, we use the `alt` keyword for the simple reason that it allows
us to apply several different rewrite rules in parallel, in a single
pass through a file:

```
sexp change '(bottomup (try (alt
    (rewrite (App $X $Y) ($X $Y))
    (rewrite (Name $X) $X)
    (rewrite (Var (Free $X)) $X)
  )))'
```

The `alt` keyword is short-circuiting, in the sense that it returns
as soon as one of the rewrite rules you pass it succeeds. Here, we don't
really take advantage of that short-circuiting: no matter what order
we put our three rules in, the program will have the same behavior.

If, however, the LHS of our alternative rewrite rules had the same
structure -- like if they all looked like `(Foo $X) RHS` or `(Foo $X)
RHS'` -- then there would at least be the chance that the order of
the rules could matter.

The select subcommand implements CSS-style selectors for traversing sexp trees

# Syntax:
# - "foo" finds the value of every pair in your tree having "foo" as the key
# - "foo bar" finds the value of every pair in your tree having "foo" as the
#   key, and then for each of these trees finds the value of every pair having
#   "bar" as the key.
# - "foo > bar" finds the value of every pair in your tree having "foo" as the
#   key, and then for each of these trees finds the value of every top-level pair
#   having "bar' as the key.
# - "*" matches anything

  $ function input {
  >   cat <<EOF
  >     ((foo bar)
  >      (baz ((sausage banana)
  >            (fred    george)
  >            (wizzle ((one   a)
  >                     (two   b)
  >                     (three c)))))
  >      (wizzle fizzle)
  >      (wizzle ((grizzle ((one z)
  >                         (two y)))
  >               (drizzle chizzle)))
  >      (fred percy))
  > EOF
  > }

  $ input | sexp select 'foo'
  bar

  $ input | sexp select 'sausage'
  banana

  $ input | sexp select 'fred'
  george
  percy

  $ input | sexp select 'baz fred'
  george

  $ input | sexp select 'two'
  b
  y

  $ input | sexp select 'wizzle two'
  b
  y

  $ input | sexp select 'wizzle > two'
  b

  $ input | sexp select 'wizzle'
  ((one a) (two b) (three c))
  fizzle
  ((grizzle ((one z) (two y))) (drizzle chizzle))

  $ input | sexp select 'wizzle > *'
  a
  b
  c
  ((one z) (two y))
  chizzle

If a sequence of sexps is input, run over each of them in turn

  $ { input; input; } | sexp select fred
  george
  percy
  george
  percy

  $ { input; input; input; } | sexp select fred
  george
  percy
  george
  percy
  george
  percy

Multiselect should group things from the same sexps

  $ input | sexp multi-select foo 'wizzle two'
  (bar b y)

  $ input | sexp multi-select -label foo 'wizzle two'
  ((foo bar) ("wizzle two" b) ("wizzle two" y))

  $ { input; input; input; } | sexp multi-select fred
  (george percy)
  (george percy)
  (george percy)


The resolve-macros transforms sexps a la [Sexplib.Macro].

  $ set -u -o pipefail

  $ echo -n "(:let f (x) a (:use x) (:use x) b) (:use f (x hello world))" > tmp.sexp; sexp resolve-macros tmp.sexp
  a
  hello
  world
  hello
  world
  b

  $ sexp resolve-macros <(echo -n "(a b)(c d)")
  (a b)
  (c d)

  $ sexp resolve-macros <(echo -n "(a b c) d e ")
  (a b c)
  d
  e

(The trailing space in this last example required here to work around
a bug.  See test-trailing-whitespace.t for more info.)

When given a pipe as its input, it doesn't silently return empty result.

  $ sexp resolve <(echo '(:use x)') |& grep 'Undefined variable'
      "Error evaluating macros: Undefined variable (included files cannot reference variables from outside)")
  [1]

It's ok even when it's many pipes, however unlikely that is.

  $ echo 'hi ' | sexp resolve <(echo '((:include /dev/stdin) (:include /dev/stdin))')
  (hi hi)

"Get" provides a compact syntax for extracting parts of an S-expression.

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.a'
  23

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.a,.b'
  23,24

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.c'
  2 3 5

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.[1]'
  b 24

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.[2]'
  c (2 3 5)

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.[2].c[0]'
  2 3 5

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.c.[1]'
  3

"Get" can ignore errors on some paths, or on all paths.

  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.a,.d?,.b'
  23,,24
  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.a,.d,.b' -ignore-errors
  23,,24
  $ echo '((a 23) (b 24) (c (2 3 5)))' | sexp get '.d,.d,.d' -ignore-errors
  ,,

"Get" runs on every S-expression given as input.

  $ sexp get '.foo' <<EOF
  > ((foo 1)(bar 2))
  > ((foo 3)
  >  (bar 4))
  > ((bar 6)
  >  (foo 5))
  > EOF
  1
  3
  5

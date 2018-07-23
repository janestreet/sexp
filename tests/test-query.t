Imported `sexp query` tests:

  $ sexp query '(test (and (field foo) (field bar)))' <<EOF
  > ((baz a) (foo 1))
  > ((baz b) (bar 2))
  > ((baz c) (foo 3) (bar 4))
  > ((baz d))
  > EOF
  ((baz c) (foo 3) (bar 4))

  $ sexp query '(and (field foo) (field bar))' <<EOF
  > ((baz a) (foo 1))
  > ((baz b) (bar 2))
  > ((baz c) (foo 3) (bar 4))
  > ((baz d))
  > EOF
  4

  $ sexp query '(branch (index 1) (equals b) none)' <<EOF
  > (a b c)
  > EOF
  b

  $ sexp query '(branch (index 9) (quote yes) (quote no))' <<EOF
  > (a b c)
  > EOF
  no

  $ sexp query '(equals woot)' <<EOF
  > woot
  > EOF
  woot

  $ sexp query '(equals (Cons 2 (Cons 5 (Cons 7 (Cons 11 ...)))))' <<EOF
  > (Cons 2 (Cons 5 (Cons 7 (Cons 11 ...))))
  > EOF
  (Cons 2 (Cons 5 (Cons 7 (Cons 11 ...))))

  $ sexp query '(not (equals (Cond 2 (Cond 5 (Cond 7 (Cond 11 ...))))))' <<EOF
  > (Cons 2 (Cons 5 (Cons 7 (Cons 11 ...))))
  > EOF
  (Cons 2 (Cons 5 (Cons 7 (Cons 11 ...))))

  $ sexp query '(if (equals xxx) (quote yes) (quote no))' <<EOF
  > xxx
  > EOF
  yes

  $ sexp query '(change lowercase)' <<EOF
  > abcd
  > Abcd
  > aBcd
  > abCd
  > abcD
  > (foo bar)
  > (FOO bar)
  > (foo BAR)
  > (Foo Bar)
  > (A (B (C (D E))))
  > Word
  > UPPERCASE
  > (A (B C) D)
  > 1234
  > EOF
  abcd
  abcd
  abcd
  abcd
  abcd
  (foo bar)
  (foo bar)
  (foo bar)
  (foo bar)
  (a (b (c (d e))))
  word
  uppercase
  (a (b c) d)
  1234

  $ sexp query '(test (or (field foo) (field bar)))' <<EOF
  > ((baz a) (foo 1))
  > ((baz b) (bar 2))
  > ((baz c) (foo 3) (bar 4))
  > ((baz d))
  > EOF
  ((baz a) (foo 1))
  ((baz b) (bar 2))
  ((baz c) (foo 3) (bar 4))

  $ sexp query '(or (field foo) (field bar))' <<EOF
  > ((baz a) (foo 1))
  > ((baz b) (bar 2))
  > ((baz c) (foo 3) (bar 4))
  > ((baz d))
  > EOF
  1
  2
  3

  $ sexp query '(or (field mc) (quote 0))' <<EOF
  > ((source wonky) (mc xxx))
  > ((source wonky) (MC xxx))
  > EOF
  xxx
  0

  $ sexp query '(pipe (index 1) (index 2) (index 0))' <<EOF
  > (((a b c) (d e f) (g h i))
  >  ((j k l) (m n o) (p q r))
  >  ((s t u) (v w x) (y z !)))
  > EOF
  p

  $ sexp query '(index 1) (index 2) (index 0)' <<EOF
  > (((a b c) (d e f) (g h i))
  >  ((j k l) (m n o) (p q r))
  >  ((s t u) (v w x) (y z !)))
  > EOF
  p

  $ sexp query '(quote (a (unquote (pipe (index 0) each)) b (unquote (pipe (index 1) each))))' <<EOF
  > ((1 2 3) (x y z))
  > EOF
  (a 1 b x)
  (a 1 b y)
  (a 1 b z)
  (a 2 b x)
  (a 2 b y)
  (a 2 b z)
  (a 3 b x)
  (a 3 b y)
  (a 3 b z)

  $ sexp query '(quote (a b c))' <<EOF
  > (1 2 3)
  > EOF
  (a b c)

  $ sexp query '(quote (a (unquote each) c))' <<EOF
  > (1 2 3)
  > EOF
  (a 1 c)
  (a 2 c)
  (a 3 c)

  $ sexp query '(quote (a (splice each) c))' <<EOF
  > (1 2 3)
  > EOF
  (a 1 2 3 c)

  $ sexp query '(wrap smash)' <<EOF
  > (a b)
  > EOF
  ((a b) a b)

  $ sexp query '(wrap each)' <<EOF
  > (a b c d)
  > EOF
  (a b c d)

The -quine flag outputs a sexp which generates itself if interpreted as a sexp query program.

  $ sexp query -quine | tee ./quine
  (pipe
   (quote
    (quote (pipe (unquote (wrap (cat (quote quote) this))) (unquote this))))
   (quote (pipe (unquote (wrap (cat (quote quote) this))) (unquote this))))

  $ echo '(arbitrary sexp)' | sexp query -file ./quine | diff ./quine -

The -quiet flag should retain exit code.

  $ echo woot | sexp query '(equals woot)' 
  woot

  $ echo woot | sexp query '(equals woot)' -quiet 

  $ echo not_woot | sexp query '(equals woot)'
  [1]

  $ echo not_woot | sexp query '(equals woot)' -quiet
  [1]

Test -count flag. -count flag does not exit 1 when count is zero.

  $ sexp query '(equals a)' -count <<EOF
  > a
  > EOF
  1

  $ sexp query '(equals a)' -count <<EOF
  > a
  > a
  > EOF
  2

  $ sexp query '(equals a)' -count <<EOF
  > b
  > EOF
  0

Variant matches variants of 0 or more arguments:

  $ sexp query '(variant foo 5)' <(echo '(foo 1 2 3 4 5)')
  (foo 1 2 3 4 5)

Both the constructor name and arity must match.

  $ sexp query '(variant foo 3)' <(echo '(foo 1 2 3 4 5)')
  [1]
  $ sexp query '(variant foo 8)' <(echo '(foo 1 2 3 4 5)')
  [1]
  $ sexp query '(variant bar 5)' <(echo '(foo 1 2 3 4 5)')
  [1]

Zero argument constructors match both with and without parens.

  $ sexp query '(variant foo 0)' <(echo 'foo')
  foo

  $ sexp query '(variant foo 0)' <(echo '(foo)')
  (foo)

You can remain silent about the arity, in which case any arity is alright.

  $ sexp query '(variant foo)' <(echo '(foo 1 2 3 4 5)')
  (foo 1 2 3 4 5)

  $ sexp query '(variant foo)' <(echo 'foo')
  foo

  $ sexp query '(variant foo)' <(echo '(foo)')
  (foo)

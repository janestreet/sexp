  $ set -u -o pipefail

# The purpose of "|| true" is to suppress SIGPIPE/EPIPE
  $ print() { printf "$@" || true; }


The trailing whitespace bug family (largely squashed!).

It's tricky to correctly handle the case when atom is the last thing
in the input.  This partitions [sexp] commands into three classes:

*** The good:

  $ print "a\nb" | sexp change id
  a
  b

  $ print "a\nb" | sexp change id /dev/stdin
  a
  b

 Not that good because it disallows trailing comments, but that's a
 different bug family.

  $ sexp select x "a"

  $ print "a\nb" | sexp pp
  a
  
  b

  $ print "a\nb" | sexp print -machine
  a
  b

  $ print "a\nb" | sexp print
  a
  b

  $ print "a\nb" | sexp query this
  a
  b

  $ print "a\nb" | sexp query this /dev/stdin
  a
  b

 Not that good, considering it exits 0, but that's a separate bug

  $ print "((a 1))\nb" | sexp to-csv |& cat
  a
  1
  
  missing value for field a

  $ print "a\nb" | sexp validate


*** The good continued (used to be bad or ugly, hurrah!):

  $ print "a\nb" | sexp resolve-macros /dev/stdin
  a
  b

  $ print "a\nb" | sexp select x

  $ print "(a x)\nb" | sexp select a
  x

  $ print "a\nb" | sexp get .
  a
  b

  $ print "a\nb" | sexp flatten
  .	a
  
  .	b
  
  $ print "a\nb\n" | sexp flatten
  .	a
  
  .	b
  

*** The ugly, but unrelated

  $ print "a\nb" | sexp restructure
  a (no-eol)

  $ print "a\nb\n" | sexp restructure
  a (no-eol)

  $ print "a\nb" | sexp restructure /dev/stdin
  a (no-eol)

  $ print "a\nb\n" | sexp restructure /dev/stdin
  a (no-eol)

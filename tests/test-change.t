Imported `sexp change` tests

  $ sexp change 'concat' <<EOF
  > (A (B C ("D E") ()) F G)
  > EOF
  "ABCD EFG"

  $ sexp change '(seq (rewrite (foo $x) (foo ($x))) (query (cat (index 0) (pipe (index 1) (change concat)))))' <<EOF
  > (foo bar)
  > EOF
  (foo bar)

  $ sexp change '(rewrite ($1 $2) ($2 $1))' <<EOF
  > ((a b) (c d))
  > EOF
  ((c d) (a b))

  $ sexp change '(rewrite ((a $1) (c $2)) ((a $1)))' <<EOF
  > ((a b) (c d))
  > EOF
  ((a b))

  $ sexp change '(rewrite ((a $1) (c ((d $2) (f ((g $3)))))) ((a $1) (c $2) (f $3)))' <<EOF
  > ((a b) (c ((d e) (f ((g h))))))
  > EOF
  ((a b) (c e) (f h))

  $ sexp change '(rewrite (((($1 $2) $3) $4) $5) ($1 $3 $5))' <<EOF
  > (((((a b) (c d)) (e f)) (g h)) (i j))
  > EOF
  ((a b) (e f) (i j))

  $ sexp change '(rewrite ($1 $2 @3) ($1 @3))' <<EOF
  > ((a b) (c d) (e f) (g h))
  > EOF
  ((a b) (e f) (g h))

  $ sexp change '(rewrite ($1 @2) $1)' <<EOF
  > (((a b) (c d) (e f)) (g h) (i j))
  > EOF
  ((a b) (c d) (e f))

  $ sexp change '(rewrite (($1 @2) @3) $1)' <<EOF
  > (((a b) (c d) (e f)) (g h) (i j))
  > EOF
  (a b)

  $ sexp change '(rewrite_record ((c $c) (a $a)) ($c $a))' <<EOF
  > ((a b) (c d))
  > EOF
  (d b)

  $ sexp change '(rewrite_record ((c (d $d)) (a $a)) ($d $a))' <<EOF
  > ((a b) (c (d e)))
  > EOF
  (e b)

  $ sexp change '(rewrite_record ((c $c) (e ((h $h) @1)) @2) ($h $c))' <<EOF
  > ((a b) (c d) (e ((f g) (h i))))
  > EOF
  (i d)

  $ sexp change '(rewrite_record ((c $c) @2) ((c x) @2))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((c x) (a b) (e f))

  $ sexp change '(rewrite_record (($1 $2) $3) ($2))' <<EOF
  > (((a b c d) (e f g h)) (i j k l))
  > EOF
  ((e f g h))

  $ sexp change '(rewrite_record ((@1) (@2)) (@1 @2))' <<EOF
  > ((a b c d) (e f g h))
  > EOF
  (a b c d e f g h)

  $ sexp change '(rewrite_record ((a $1) @2) (@2))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((c d) (e f))

  $ sexp change '(rewrite_record ((c $1) @2) (@2))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((a b) (e f))

  $ sexp change '(rewrite_record (@1) ((g h) @1))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((g h) (a b) (c d) (e f))

  $ sexp change '(rewrite_record ((c $1) @2) ((c g) @2))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((c g) (a b) (e f))

  $ sexp change '(rewrite_record ((c $1) @2) ((g h) @2))' <<EOF
  > ((a b) (c d) (e f))
  > EOF
  ((g h) (a b) (e f))

  $ sexp change '(rewrite_record ((c $c) (e $e) @l) ((ce ($c $e)) @l))' <<EOF
  > ((a b) (c d) (e f) (g h))
  > EOF
  ((ce (d f)) (a b) (g h))

  $ sexp change '(rewrite (begin @x end) @x)' <<EOF
  > (begin 1 2 3 end)
  > EOF
  (1 2 3)

  $ sexp change '(rewrite (begin @x 2 @y end) @x)' <<EOF
  > (begin 1 2 3 end)
  > EOF
  Error parsing command line.  Run with -help for usage information.
  failed to parse QUERY value "(rewrite (begin @x 2 @y end) @x)"
  (Failure "duplicate list variables @x @y in the same list")
  [1]

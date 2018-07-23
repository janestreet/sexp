One can change record-sexps into CSV format using `sexp to-csv`.

  $ sexp to-csv <<EOF
  > ((foo 1) (bar u) (baz Q))
  > ((foo 2) (bar w) (baz QQ))
  > ((foo 3) (bar x) (baz QQQ))
  > ((foo 4) (bar y) (baz QQQQ))
  > ((foo 5) (bar z) (baz QQQQQ))
  > EOF
  foo,bar,baz
  1,u,Q
  2,w,QQ
  3,x,QQQ
  4,y,QQQQ
  5,z,QQQQQ

Use the -d flag to change the delimiter.

  $ sexp to-csv -d \| <<EOF
  > ((foo 1) (bar u) (baz Q))
  > ((foo 2) (bar w) (baz QQ))
  > ((foo 3) (bar x) (baz QQQ))
  > ((foo 4) (bar y) (baz QQQQ))
  > ((foo 5) (bar z) (baz QQQQQ))
  > EOF
  foo|bar|baz
  1|u|Q
  2|w|QQ
  3|x|QQQ
  4|y|QQQQ
  5|z|QQQQQ

Missing fields lead to complaints.

  $ sexp to-csv <<EOF
  > ((foo 1) (bar u) (baz Q))
  > ((foo 2)         (baz QQ))
  > (        (bar x) (baz QQQ))
  > ((foo 4) (bar y)           )
  > ()
  > EOF
  foo,bar,baz
  1,u,Q
  2,,QQ
  ,x,QQQ
  4,y,
  ,,
  missing value for field bar
  missing value for field foo
  missing value for field baz
  missing value for field foo
  missing value for field bar
  missing value for field baz

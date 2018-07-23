
Make sure we're not running [/usr/bin/sexp].

  $ which sexp
  */.jenga.tmp/*/sexp (glob)

Note: we also need to be careful that jenga doesn't remove our binary
while we're testing, and then we fall back to using /usr/bin/sexp
anyway.  There is a check in the [run-tests] script to guard against
this.

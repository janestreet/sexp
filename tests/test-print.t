
By default, sexp print accepts as many valid sexps as possible off
stdin and silently ignores any malformed sexps aftrwards.

  $ echo 'a"b' | sexp print
  a

To error out in this case, pass the -fail-on-parse-error flag.

  $ echo 'a"b' | sexp print -fail-on-parse-error 2>&1 | head -n 6
  a
  Uncaught exception:
    
    (Failure "Sexplib.Lexer.scan_string: unterminated string at line 1 char 1")
  
  (Called from|Raised at|Re-raised at) file ".*"( \(inlined\))?, line [0-9]+, characters .* (re)

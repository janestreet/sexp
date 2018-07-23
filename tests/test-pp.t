Test that iterpret-atoms preserves quoted atoms

  $ echo '(message "some message")' | sexp pp
  (message "some message")

  $ echo '(message "some message")' | sexp pp -i
  (message "some message")

Now the same for failwiths-like input

  $ echo 'Error was: (message "some message")' | sexp pp
  Error
  
  was:
  
  (message "some message")


  $ echo 'Error was: (message "some message")' | sexp pp -i
  Error
  
  was:
  
  (message "some message")

  $ echo '"Error was: (message \"some message\")"' | sexp pp
  "Error was: (message \"some message\")"

  $ echo '"Error was: (message \"some message\")"' | sexp pp -i
  ("Error was:" (message "some message"))

Lets check that spaces are preserved

  $ echo '"   spaces   "' | sexp pp
  "   spaces   "

  $ echo '"   spaces   "' | sexp pp -i
  "   spaces   "

  $ echo '(message "   spaces   ")' | sexp pp
  (message "   spaces   ")

  $ echo '(message "   spaces   ")' | sexp pp -i
  (message "   spaces   ")

Note that in -i mode in case of atom concatenation, spaces are lost.

  $ echo '"What if  there are    spaces ?: (message \"and a sexp\")"' | sexp pp
  "What if  there are    spaces ?: (message \"and a sexp\")"

  $ echo '"What if  there are    spaces ?: (message \"and a sexp\")"' | sexp pp -i
  ("What if there are spaces ?:" (message "and a sexp"))

Lets check that unnecessary quotes are removed (note that this is in line with 'sexp print')

  $ echo '(contents "message")' | sexp pp
  (contents message)

  $ echo '(contents "message")' | sexp pp -i
  (contents message)

Comments

  $ (echo "; Comment"; echo '(a sexp)') | sexp pp
  ; Comment
  (a sexp)

  $ (echo "; Comment"; echo '(a sexp)') | sexp pp -i
  ; Comment
  (a sexp)

  $ (echo '(a sexp)'; echo "; Comment"; echo '(b sexp)') | sexp pp
  (a sexp)
  
  ; Comment
  (b sexp)



  $ (echo '(a sexp)'; echo "; Comment"; echo '(b sexp)') | sexp pp -i
  (a sexp)
  
  ; Comment
  (b sexp)

Special symbols

  $ echo '(contents "\*\*\*cash")' | sexp pp
  (contents "\\*\\*\\*cash")
  $ echo '(contents "\*\*\*cash")' | sexp pp -i
  (contents "\\*\\*\\*cash")

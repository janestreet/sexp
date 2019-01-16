let query_by_example_dot_md =
  "---\ntitle: Sexp query by example\nparent: ../README.md\n---\n\nSexp query is a comma\
   nd-line tool for getting data out of S-expressions. Really it's a\nmini programmi\
   ng language, and if you want to use it effectively you'll want to see lots of\nex\
   amples. That's what this README is for.\n\nTable of contents\n=================\n\n- \
   Basic sexp query commands and how they work\n    - Field\n    - Index\n    - Equals\
  \ and Test\n    - Pipe\n    - Each\n    - Smash\n    - Regex\n    - Cat and Wrap\n- \"se\
   xp select\" and \"sexp multi-select\"\n\nBasic sexp query commands and how they work\n\
   ==============================================\n\nThe basic building blocks of a s\
   exp query are the commands `field`, `index`, `equals`,\n`test`, `pipe`, `each`, `\
   smash`, `regex`, `cat`, and `wrap`. (There are a few other\ncommands but they're \
   less important.) We'll explain each of these in turn before getting\ninto more co\
   mplicated examples.\n\nField\n-----\n\n`field` gets the value from a `(key value)` pa\
   ir. Suppose you've got a record with two\nfields, `jane_symbol` and `bloomberg`. \
   Well, calling `(field jane_symbol)` gets you the\nvalue of that field:\n\n```sh\n  $\
  \ echo '((jane_symbol \"AAPL US\") \\\\\n           (bloomberg \"AAPL UW Equity\"))' | s\
   exp query '(field jane_symbol)'\n  # => \"AAPL US\"\n```\n\nIndex\n-----\n\n`index` is a \
   little dumber, in that it just gives you the nth element in a list, indexed\nfrom\
  \ 0:\n\n```sh\n  $ echo \"(one two three four five)\" | sexp query '(index 2)'\n  # => \
   three\n```\n\nIf you've got a single-element list, like `(one)`, calling `(index 0)\
   ` on it is a nifty\ntrick for removing the parens.\n\nEquals and Test\n-------------\
   --\n\n`equals` and `test` are often used together, because `equals` alone is rarel\
   y what you\nwant. It just returns a value if the value is equal to some string, a\
   nd nothing otherwise.\nSuppose we had a little corpdir expressed as an S-expressi\
   on (or really, a series of\nexpressions, each separated by a blank line):\n\n```oca\
   ml\n  ;; ./corpdir.sexp\n\n  ((name ((first Bill) (last Nye)))\n   (role \"Science gu\
   y\")\n   (start_date ((year 2012) (month 3) (day 4))))\n\n  ((name ((first Zadie) (l\
   ast Smith)))\n   (role \"Author\")\n   (start_date ((year 2016) (month 10) (day 21))\
   ))\n```\n\nNow we can run some `equals` checks against it. Notice below how the `eq\
   uals` expression\nfollows the `(field role)` expression; that's because it's oper\
   ating on the output of\n`(field role)`, as if piping the results of the first com\
   mand into the second. (We'll see\nhow this works under the hood once we get to th\
   e `pipe` command.)\n\n```sh\n  $ cat corpdir.sexp | sexp query '(field role) (equal\
   s \"Author\")'\n  # => Author\n\n  $ cat corpdir.sexp | sexp query '(field role) (equ\
   als \"Science guy\")'\n  # => \"Science guy\"\n\n  $ cat corpdir.sexp | sexp query '(fi\
   eld role) (equals \"Foo\")'\n  # =>\n```\n\nYou can see why this isn't very useful: in\
  \ general when we test a record for something, we\nwant to *do* something with tha\
   t record. But here we just return the value we're testing\nagainst. This is where\
  \ `test` comes in:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(test (field role) (\
   equals \"Author\"))'\n  # => ((name ((first Zadie) (last Smith))) (role Author)\n  #\
  \ => (start_date ((year 2016) (month 10) (day 21))))\n\n  $ cat corpdir.sexp | sexp\
  \ query \\\\\n      '(test (field role) (equals \"Author\")) (field start_date)'\n  # =\
   > ((year 2016) (month 10) (day 21))\n```\n\n`test` wraps around a condition, like `\
   (equals 'foo')`, and if the condition passes, it\nreturns the entire S-expression\
  \ satisfying the condition. You can then pass this to the\nright -- here, to the `\
   (field start_date)` operator -- to get at some field within that\nreturned value.\
  \ A lot of sexp queries work this way: first you filter for records\nsatisfying a \
   condition, and then you dig in to those looking for specific data.\n\nPipe\n----\n\n`\
   pipe` gets its name from the Unix command-line `|` that passes output from one p\
   rogram to\nanother. It's a way of chaining together a sequence of commands, like \
   a\n`Sequence.concat_map` for sexp queries.\n\nIn many cases you don't actually have\
  \ to write the pipe, because it's already there\nimplicitly. For instance this que\
   ry:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(field start_date) (field day)'\n  \
   # => 4\n  # => 21\n```\n\ntakes the result of the first command, `(field start_date)\
   `, and implicitly pipes it to\nthe next, `(field day)`. It's as if there's a lite\
   ral `|` pipe character between the two\nstatements. Writing it with the actual pi\
   pe command gives:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(pipe (field start_d\
   ate) (field day))'\n  # => 4\n  # => 21\n```\n\nOne pipe command can take an arbitrar\
   y number of sub-statements, not just 2, as in the\nfollowing example:\n\n```sh\n  $ \
   cat corpdir.sexp | sexp query '(pipe (field start_date) (field day) (equals 4))'\
   \n  # => 4\n```\n\nIt's worth asking: if there's syntactic sugar to get rid of these\
  \ explicit pipes, do you\never really need the `pipe` command? In fact you do. Whi\
   le some commands, like `test`, can\ntake a series of substatements without requir\
   ing a pipe, others, like `cat` and `unquote`,\nwhich we'll see later, require it.\
  \ So you'll see `pipe` all over, usually in places where\nyou have a complex sub-q\
   uery, i.e., a query that involves more than a single `(field ...)`\ncommand.\n\nEac\
   h\n----\n\n`each` is pretty simple: it takes each element in a list and passes them\
  \ one by one to\nanother expression. At the top level, you can use it like:\n\n```sh\
   \n  <list> each <expression>\n```\n\nas in the following example:\n\n```sh\n  $ cat cor\
   pdir.sexp | sexp query \\\\\n      '(test (field role) (equals \"Science guy\")) (fie\
   ld name) each (index 1)'\n  # => Bill\n  # => Nye\n```\n\nIt's worth dwelling a bit o\
   n what's happening here. On the left-hand side of the `each`,\nyou have an expres\
   sion that returns the `name` field of the record where the role field\nhas the va\
   lue \"Science guy\". So what you're passing to the `each` is the list:\n\n```sh\n  ((\
   first Bill) (last Nye))\n```\n\nwhich has two elements. (Notice how our little comm\
   and-line program returns two lines.)\nThen, the right-hand side of the `each` is \
   just an expression that operates on each\nelement of the list, so on `(first Bill\
   )` and `(last Nye)` in turn. `(index 1)` returns\nthe second element of whatever \
   it's passed, which is how we end up with \"Bill\" and \"Nye\".\n\n(`each` appears to b\
   e an infix operator because of the implicit pipe\nat the top level. But if you we\
   re to use it inside of a `test`, for\nexample, as in `(test (pipe (field hosts) e\
   ach)`, you must explicitly\npipe the output of `field` to the `each`.)\n\nSmash\n---\
   --\n\nsmaaaaasssshhhh!!!! This one has the coolest name, and also, in a way, the c\
   oolest\nbehavior: It takes an S-expression and returns every sub-expression of it\
   . Then, like\n`each`, it lets you apply a command to every one of those sub-expre\
   ssions. So it also uses\nthat infix-style `<expression> smash <expression>` synta\
   x. But let's see what it looks\nlike when we operate on the whole _corpdir.sexp_ \
   file, without actually doing anything\nwith the smashed contents:\n\n```sh\n  $ cat \
   corpdir.sexp | sexp query 'smash'\n  # => ((name ((first Bill) (last Nye))) (role\
  \ \"Science guy\")\n  # =>  (start_date ((year 2012) (month 3) (day 4))))\n  # => (na\
   me ((first Bill) (last Nye)))\n  # => name\n  # => ((first Bill) (last Nye))\n  # =\
   > (first Bill)\n  # => first\n  # => Bill\n  # => (last Nye)\n  # => last\n  # => Nye\
   \n  # => (role \"Science guy\")\n  # => role\n  # => \"Science guy\"\n  # => (start_date\
  \ ((year 2012) (month 3) (day 4)))\n  # => start_date\n  # => ((year 2012) (month 3\
   ) (day 4))\n  # => (year 2012)\n  # => year\n  # => 2012\n  # => (month 3)\n  # => mo\
   nth\n  # => 3\n  # => (day 4)\n  # => day\n  # => 4\n  #\n  # => ((name ((first Zadie)\
  \ (last Smith))) (role Author)\n  # =>  (start_date ((year 2016) (month 10) (day 2\
   1))))\n  # => (name ((first Zadie) (last Smith)))\n  # => name\n  # => ((first Zadi\
   e) (last Smith))\n  # => (first Zadie)\n  # => first\n  # => Zadie\n  # => (last Smi\
   th)\n  # => last\n  # => Smith\n  # => (role Author)\n  # => role\n  # => Author\n  # \
   => (start_date ((year 2016) (month 10) (day 21)))\n  # => start_date\n  # => ((yea\
   r 2016) (month 10) (day 21))\n  # => (year 2016)\n  # => year\n  # => 2016\n  # => (\
   month 10)\n  # => month\n  # => 10\n  # => (day 21)\n  # => day\n  # => 21\n```\n\nWhat'\
   s going on here? Well, since we passed the whole file to `smash`, rather than ju\
   st a\nsingle record, we're getting the smashed contents of each of our two record\
   s in turn (one\nfor Bill Nye and one for Zadie Smith). For each of these, the com\
   mand is coughing up every\nsub-expression of the original record. You can think o\
   f it as taking anything of the form\n`<left> <right>` and printing `<left> <right\
   >`, `<left>`, and `<right>`. Since\nS-expressions can be deeply nested, this can \
   end up printing a lot of stuff.\n\nSmashing is useful when you don't want to do a \
   million chained tests in order to get to\nsome record nested deep in an S-express\
   ion. For instance, suppose our day records were\nburied in a lot of other stuff, \
   like so:\n\n```ocaml\n  ;; ./corpdir.sexp\n\n  ((name ((first Bill) (last Nye)))\n   (\
   role \"Science guy\")\n   (start_date ((year 2012) (month 3) (period ((unit ((kind \
   ((sol 400) (day 4))))))))))\n\n  ((name ((first Zadie) (last Smith)))\n   (role \"Au\
   thor\")\n   (start_date ((year 2016) (month 10) (period ((unit ((kind ((sol 2100) \
   (day 21))))))))))\n```\n\nIf you knew you wanted to get at those `(day <num>)` reco\
   rds, you could write something\nlike:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(\
   field start_date) (field period) (field unit) \\\\\n                               \
  \      (field kind) each (test (index 0) (equals day))'\n  # => (day 4)\n  # => (da\
   y 21)\n```\n\nor... you could just smash the input and filter on the field name:\n\n`\
   ``sh\n  $ cat corpdir.sexp | sexp query 'smash (test (index 0) (equals day))'\n  #\
  \ => (day 4)\n  # => (day 21)\n```\n\nKeep in mind that using `smash` isn't free: the\
   re's a tradeoff between being concise and\nbeing precise when deciding whether to\
  \ use it. That is, while it may be powerful for\nfinding deeply nested things, tha\
   t's only because you've given up some control over where\nthe thing is to be foun\
   d. In a way, `smash` is the sexp query analog of `.*` in regular\nexpressions. It\
  \ should be used with caution.\n\nRegex\n-----\n\n`regex` is like `equals`, except tha\
   t instead of taking a simple string argument, it takes\na regular expression, so \
   that you can do slightly more versatile searching. Let's say we\nhad a new hire i\
   n our corpdir:\n\n```ocaml\n  ;; ./corpdir.sexp\n\n  ((name ((first Bill) (last Nye))\
   )\n  (role \"Science guy\")\n  (start_date ((year 2012) (month 3) (day 4))))\n\n  ((na\
   me ((first Zadie) (last Smith)))\n  (role \"Author\")\n  (start_date ((year 2016) (m\
   onth 10) (day 21))))\n\n  ((name ((first David) (last Lynch)))\n  (role \"Auteur\")\n \
  \ (start_date ((year 2017) (month 5) (day 20))))\n```\n\nIf we then wanted to get th\
   e name records of everyone whose role starts with \"Au\", we\ncould use regex to do\
  \ it:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(test (field role) (regex \"^Au\"))\
  \ (field name)'\n  # => ((first Zadie) (last Smith))\n  # => ((first David) (last L\
   ynch))\n```\n\nBy default, `regex` will return the entire string if there's a match\
   , and nothing if not;\nbut if you use a capture group, as in the following exampl\
   e, it'll return the capture\ngroup's contents instead. (If you supply multiple ca\
   pture groups it'll return the result\nof the first one.) For instance:\n\n```sh\n  $\
  \ cat corpdir.sexp | sexp query '(field role) (regex \"^Au(.*)\")'\n  # => thor\n  # \
   => teur\n```\n\nCat and Wrap\n------------\n\n`cat` is how you run multiple commands o\
   n a single S-expression at one time,\ncon-`cat`-enating the results. Where `pipe`\
  \ is a way of combining sub-queries in series,\n`cat` combines them in parallel. S\
   o for example:\n\n```sh\n  $ cat corpdir.sexp | sexp query '(cat (field name) (fiel\
   d role))'\n  # => ((first Bill) (last Nye))\n  # => \"Science guy\"\n  # => ((first Z\
   adie) (last Smith))\n  # => Author\n  # => ((first David) (last Lynch))\n  # => Aut\
   eur\n```\n\nNotice how for each record we've fetched both the name and the role. Bu\
   t also notice how\nthe results aren't wrapped up into a single S-expression. That\
   's where `wrap` comes in --\nit's a command that simply takes some stuff and wrap\
   s it in parens, and it's frequently\nused together with `cat`:\n\n```sh\n  $ cat cor\
   pdir.sexp | sexp query '(wrap (cat (field name) (field role)))'\n  # => (((first \
   Bill) (last Nye)) \"Science guy\")\n  # => (((first Zadie) (last Smith)) Author)\n  \
   # => (((first David) (last Lynch)) Auteur)\n```\n\nNow the results of our multiple \
   queries are nicely wrapped up into a single S-expression\nper record.\n\n`sexp sele\
   ct` and `sexp multi-select`\n=====================================\n\nA lot of the \
   time, what would be a fairly complicated sexp query is more easily expressed\nas \
   a sexp multi-select. Suppose you wanted to pull out the actual day, month, and y\
   ear of\neach person in our little corpdir. You could do it using sexp query:\n\n```\
   sh\n  $ cat corpdir.sexp | sexp query '(field start_date) \\\\\n                    \
  \                 (wrap (cat (field day) (field month) (field year)))'\n  # => (4 \
   3 2012)\n  # => (21 10 2016)\n  # => (20 5 1999)\n```\n\nBut it's actually much easie\
   r when expressed as a multi-select:\n\n```sh\n  $ cat corpdir.sexp | sexp multi-sel\
   ect day month year\n  # => (4 3 2012)\n  # => (21 10 2016)\n  # => (20 5 1999)\n```\n\
   \nThis multi-select does a kind of `smash`, `cat`, and `wrap` on the fields that \
   you pass to\nit. Notice that you don't even need to quote your field names!\n\nBeca\
   use it's more or less doing a smash, the same caveat applies: multi-select is a\n\
   concise way to find things, but at the expense of being somewhat imprecise about\
  \ where\nyou're looking.\n"
;;

let query_semantics_dot_md =
  "---\ntitle: Sexp query formal semantics\nparent: ../README.md\n---\n\nSee also the ge\
   t and select subcommands.\n\nThis document explains the semantics of query express\
   ions, whose\nsyntax is as follows:\n\n    E ::=\n        -- selection -------------\n\
  \        | (index NUM)\n        | (field STRING)\n        | each\n        | smash\n  \
  \      -- composition -----------\n        | (pipe E E ...)\n        | (cat E E ...\
   )\n        | this\n        | none\n        -- tests/conditionals ----\n        | ato\
   mic\n        | (variant TAG NUM)\n        | (equals SEXP ...)\n        | (regex R)\n\
  \        | (test E E ...)\n        | (not E)\n        | (and E E ...)\n        | (or\
  \ E E ...)\n        | (if E E E)\n        | (branch E E E)\n        -- formatting --\
   ----------\n        | (wrap E)\n        | (quote T[0])\n        -- transformations \
   -------\n        | (change C)\n        | restructure\n\nThe meaning of an expression\
  \ is a function from an input s-expression to a\n(possibly empty) sequence of outp\
   ut s-expressions.  The toplevel command is\napplied to each expression in the inp\
   ut.  We will look at five categories\nof expressions: selection, composition, con\
   ditionals, formatting, and\ntransformation.\n\nSelection\n---------\n\nA `(index N)` e\
   xpression picks out the Nth element of a list s-expression.\nIt succeeds if N is \
   a proper index (i.e., 0 <= N < list length) and fails\notherwise.  Upon success, \
   it returns the single-element sequence containing\nthe selected sub-expressions. \
  \ Upon failure, it returns an empty list.\n\n    (index 2) : (one two three four) =\
   > {three}\n    (index 8) : (one two three four) => {}\n\nA `(field F)` expression i\
   s like `(index N)` except that it projects\nthe field named F out of a record.  I\
   f more than one field of that\nname exists, they will all be selected.\n\n    (fiel\
   d foo) : ((bar 1) (foo 2) (baz 3))         => {2}\n    (field foo) : ((bar 1) (fo\
   o 2) (baz 3) (foo 4)) => {2, 4}\n    (field wow) : ((bar 1) (foo 2) (baz 3))     \
  \    => {}\n\nAn `each` expression selects every element of a list.\n\n    each : (on\
   e two three four) => {one, two, three, four}\n    each : ()    => {}\n    each : h\
   ello => {}\n\nAn `smash` expression selects every sub-expression of an s-expressio\
   n\n\n    smash : (a (b c) (d (e f))) => { (a (b c) (d (e f))),\n                   \
  \                   a, (b c), (d (e f)),\n                                        \
  \  b, c,  d, (e f),\n                                                     e, f }\n\n\
   Composition\n-----------\n\nThe two basic composition operators are `pipe` and `cat\
   `.  These two\noperators do sequential and parallel composition, respectively.  T\
   he\ndenotation of\n\n                    (pipe E1 E2 ... En)\n\nis a \"fanned out\" com\
   position of the denotations D = [E1] and\nD' = [`(pipe E2 ... En)`] in which D' i\
   s called on every s-expression\nin the output sequence of D and all the resulting\
  \ sequences of\ns-expressions are concatenated together into one big sequence.\n\n  \
  \          (pipe E) = E\n            (pipe E1 E2 ... En) : S => X1 U ... U Xm\n\n   \
  \         where   E1 : S => {S1, ..., Sm}\n              and   (pipe E2 ... En) : \
   Si => Xi   for i in {1..m}\n\nIn particular, this means that if D returns an empty\
  \ sequence, then\nD' will never be called.  The denotation of\n\n                   \
  \ (cat E1 E2 ... En)\n\nis simply a parallel execution of each Ei on the input X in\
  \ which all\n\n            (cat E1 ... En) : S => X1 U ... U Xn\n\n            where \
  \  Ei : S => Xi    for i in {i .. n}\n\nThere are two trivial expressions, `this` a\
   nd `none`.  A this expression\nsimply selects it input, while a `none` expression\
  \ selects nothing\n\n                this : S => {S}         none : S => {}\n\nThese \
   are identities for the pipe and cat operators.\n\n                this = (pipe)   \
  \       none = (cat)\n\n(For monad fans -- pipe is Kleisli composition and `this` i\
   s Kleisli\nidentity for the list monad.)\n\n\nConditionals\n------------\n\nFor the pur\
   pose of conditional execution, we treat an expression returning\na non-empty sequ\
   ence as true, and an expression returning an empty sequence\nas false.  With this\
  \ in mind, one may think of `this` and `none` as the\nconstants true and false, re\
   spectively.\n\nA `atomic` expression tests for atomic s-expressions.\n\n        atom\
   ic : foo       => {foo}\n        atomic : (foo bar) => {}\n\nThe most basic non-tri\
   vial condition is equality.  The expression\n`(equals S)` selects its input in th\
   e case that the (returns true) and\nfails (returns false) otherwise.\n\n        (eq\
   uals S) : S' => {S'}    if S = S'\n        (equals S) : S' => {}      otherwise\n\n\
   There is also a N-ary version of equals that is expands to a disjunction\n\n      \
  \  (equals S1 S2 ...) = (or (equals S1) (equals S2) ...)\n\nA `(regex R)` expressio\
   n tests for the s-expresion to be an ATOM that\nmatches R.  The truth value will \
   be the first capturing group of the\nregular expression or the whole atom if regu\
   lar expression has no\ncapturing groups.\n\nA `(variant F N)` expression tests for \
   the s-expression form of an element\nof a variant type with constructor F applied\
  \ to N arguments.  You can leave N off, in\nwhich case it will match a variant wit\
   h any number of arguments.\n\n    (variant foo 5) : (foo 1 2 3 4 5) => {(foo 1 2 3\
  \ 4 5)}\n    (variant foo 3) : (foo 1 2 3 4 5) => {}\n    (variant foo 8) : (foo 1 \
   2 3 4 5) => {}\n    (variant bar 5) : (foo 1 2 3 4 5) => {}\n    (variant foo 0) :\
  \ foo => {foo}\n    (variant foo 0) : (foo) => {(foo)}\n    (variant foo)   : (foo \
   1 2 3 4 5) => {(foo 1 2 3 4 5)}\n    (variant foo)   : foo => {foo}\n    (variant \
   foo)   : (foo) => {(foo)}\n\nA `(test E)` expression proceeds by evaluating E on t\
   he current s-expression\nS and selecting S only in the case that E succeeds.\n\n   \
  \     (test E) : S => {}    if X empty      where  E : S => X\n        (test E) : \
   S => {S}   otherwise\n\nWe also provide `(test E1 E2 ...)` as syntactic sugar for \
   the common\nidiom `(test (pipe E1 E2 ...))`.\n\nA `(not E)` expression proceeds by \
   evaluating E on the current s-expression\nS and selecting S only in the case that\
  \ E fails.\n\n        (not E) : S => {S}    if X empty      where  E : S => X\n     \
  \   (not E) : S => {}     otherwise\n\nNote that X is is discarded from the output \
   of both (test E) and (not E).\nFor this reason, these two operators are useful fo\
   r \"looking down\" into an\ns-expression while remembering your place at some point\
  \ above where you will\nreturn later.\n\nAn `(and E1 ... En)` expression does short-\
   circuit evaluation based on whether\nor not E1 succeeds.  Upon success, it return\
   s the results of En.\n\n    (and) = this\n\n    (and E) = E\n\n    (and E1 E2 ... En) \
   : S => {}        if  E1 : S => {}\n    (and E1 E2 ... En) : S => Y         if  E1\
  \ : S => X  (nonempty)\n                                        and (and E2 ... En\
   ) : S => Y\n\nAn `(or E1 ... En)` expression does short-circuit evaluation based o\
   n whether\nor not E1 succeeds.  It returns the results of the first Ei that succe\
   eds.\n\n    (or) = none\n\n    (or E) = E\n\n    (or E1 E2 ... En) : S => X         if\
  \  E1 : S => X  (nonempty)\n    (or E1 E2 ... En) : S => Y         if  E1 : S => {\
   }\n                                       and (or E2 ... En) : S => Y\n\nAn `(if E1\
  \ E2 E3)` expression does conditional execution of E2 or E3 based\non whether or n\
   ot E1 succeeds.\n\n    (if E1 E2 E3) : S => X2      if E1 : S => X (non-empty) and\
   \n    (if E1 E2 E3) : S => X3      if E1 : S => {}\n\n       where   Ei : Si => Xi \
  \   for i in {2,3}\n\nAn `(branch E1 E2 E3)` expression does conditional execution \
   like `if`, but\nalso pipes the output of the condition into the `then' branch.\n\n \
  \   (branch E1 E2 E3) : S => X1 U ... U Xn\n\n                                    i\
   f E1 : S => {S1, ... , Sn} (non-empty)\n                                    and E\
   2 : Si => Xi  for i in {1..n}\n\n    (branch E1 E2 E3) : S => X3\n                 \
  \                   if E1 : S => {}\n                                    and E3 : \
   S3 => X3\n\nThe following equations relating the behavior of `if` and `branch` are\
  \ true:\n\n    (if E1 E2 E3) = (branch (test E1) E2 E3)\n\n    (branch E1 E2 E3) = (i\
   f E1 (pipe E1 E2) E3)\n\n\nFormatting\n----------\n\nUsing the commands so far, one ma\
   y only output sub-expressions of the input\nexpression itself.  However, we may a\
   lso want to impose additional structure\non the output.\n\nA `(wrap E)` expression \
   runs E and gathers up the resulting sequence into a\nsingle list s-expression tha\
   t becomes the (single) overall result.\n\n    (wrap E) : S => {(S1 ... S2)}  where\
  \  E : S => {S1, ... , S2}\n\nNote that the final output sequence has exactly one e\
   lement that is a list.\n\nA `(quote S)` expression adds the provided s-expression \
   to the manifest.\n\n    (quote S') : S => {S'}\n\nNote that S is discarded here.  Fo\
   r this reason, quote is often used in\nconjunction with a parallel composition op\
   erator like `cat`, `and`, or `or`.\n\n    (quote (a b c))              : (1 2 3) =\
   > {(a b c)}\n    (quote (a (unquote each) c)) : (1 2 3) => {(a 1 c), (a 2 c), (a \
   3 c)}\n    (quote (a (splice each) c))  : (1 2 3) => {(a 1 2 3 c)}\n\n    (quote (a\
  \ (splice each) c (unquote each)))\n        : (1 2 3) => {(a 1 2 3 c 1), (a 1 2 3 \
   c 2), (a 1 2 3 c 3)}\n\nMultiple unquotes in a single quoted template yield a cart\
   esian product.\n\n    (quote (a (unquote (pipe (index 0) each))\n            b (unq\
   uote (pipe (index 1) each))))\n        : ((1 2 3) (x y z)) => {(a 1 b x), (a 1 b \
   y), (a 1 b z),\n                                (a 2 b x), (a 2 b y), (a 2 b z),\n\
  \                                (a 3 b x), (a 3 b y), (a 3 b z)}\n\nFurthermore, n\
   ested quotes increase the \"degree\" of quotation.  Splice and\nunquote only have t\
   heir effect at the degree zero.  This feature is intended\nto facilitate sexpquer\
   y expressions that manipulate other sexpquery expressions.\n\n--- grammar for sexp\
   query templates ---\n\n          T[0] ::= ATOM               T[n+1] ::= ATOM\n     \
  \            | (T[0] ... T[0])             | (T[n+1] ... T[n+1])\n                \
  \ | (quote T[1])                | (quote T[n+2])\n                 | (unquote E)  \
  \               | (unquote T[n])\n                 | (splice E)                  |\
  \ (splice T[n])\n\nTransformations\n---------------\n\nFinally, we have a way to call \
   the change language from the query\nlanguage.  The semantics of (change C) are to\
  \ return the transformed\nexpression as a singleton sequence and propagate failure\
   .\n\n    (change C) : S => {S'}    if C : S => S'\n    (change C) : S => {}      if\
  \ C : S => _|_\n\n(see the change subcommand's internal documentation for informati\
   on on\nchange semantics).\n\nSometimes the contents of an atom will be the string r\
   epresentation of\na sequence of sexps. `restructure` will do this interpretation \
   for you:\n\n    restructure : \"A (B C) D\" => {A (B C) D}\n"
;;

let change_by_example_dot_md =
  "---\ntitle: Sexp change by example\nparent: ../README.md\n---\n\nSexp change is a com\
   mand-line tool for manipulating S-expressions,\nuseful if you want to do some dat\
   a munging without having to write a\nfull-fledged program to parse and process yo\
   ur sexps.\n\nThis doc collects examples of sexp change in action. For a more\ncompl\
   ete overview of the language, see\n[this doc](./change_semantics.md),\nwhich inclu\
   des many small examples showing how to use each keyword.\n\nApplying a rewrite rul\
   e throughout a file\n-----------------------------------------\n\nPerhaps the most \
   common way to use `sexp change` is just to barrel\nthrough a file attempting to a\
   pply a rewrite rule. You can accomplish\nthis with the following:\n\n```\n(topdown (\
   try (rewrite LHS RHS)))\n```\n\nThe `topdown` keyword ensures you cover the entire \
   file, while `try`\nensures that if you can't apply the rewrite rule to a given ex\
   pression\nLHS, you leave LHS as it is.\n\nUsing sexp query and sexp change together\
   \n-----------------------------------------\n\nHere are a pair of examples making u\
   se of sexp query's `index`,\n`pipe`, `quote` and `unquote` commands, and sexp cha\
   nge's `seq`,\n`rewrite`, and `concat` commands.\n\nSuppose your input is something \
   like:\n\n```\n(AMZN ( ... ))\n(MSFT ( ... ))\n```\n\nand you want to change it to:\n\n```\
   \n(\"AMZN US\" ( ... ))\n(\"MSFT US\" ( ... ))\n```\n\nHere's how you could do it using t\
   he `change` subcommand of `sexp query`:\n\n```sh\nsexp query '\n(quote (\n  (unquote \
   (pipe (index 0) (change (seq (rewrite $X ($X \" US\")) concat))))\n  (unquote (inde\
   x 1))))\n' <<EOF\n(AMZN (foo bar))\n(MSFT (foo bar))\nEOF\n```\n\nLet's break down how \
   this works. From\n[the sexp query readme](query_by_example.md) you know how to us\
   e\n`quote` to create an S-expression \"template\" that is then filled in\nwith `unqu\
   ote` statements. Here, that template is going to look like\n`(A B)`, where the `A\
   ` part is filled in by the results of:\n\n```sh\n(pipe (index 0) (change (seq (rewr\
   ite $X ($X \" US\")) concat)))\n```\n\nWe pipe the results of `(index 0)`, i.e., some\
   thing like \"AMZN\", to\nthe `change` command, which consists of a `seq`-uence of t\
   wo\nsubcommands: `(rewrite $X ($X \" US\"))` takes the single S-expression\nlike `AM\
   ZN` (an atom) and rewrites it as `(AMZN \" US\")`. `concat` then\nconcatenates this\
  \ 2-atom list into the single atom `\"AMZN US\"`.\n\nFinally, the second half of the \
   `quote` statement plops the result of\n`(unquote (index 1))` into the latter half\
  \ of the list, creating the\ndesired `(AMZN (foo bar))`.\n\nSuppose we wanted to do \
   this for sexps of more than two items, like\nso:\n\n```\n(AMZN ( ... ) ( ... ))\n(MSF\
   T ( ... ) ( ... ) ( ... ))\n```\n\nThen you'd want to use the `query` subcommand of\
  \ `sexp change`:\n\n```sh\nsexp change '\n(seq\n  (rewrite ($A @B) ($A (@B)))\n  (query\
  \ (\n    quote (\n      (unquote (pipe (index 0) (change (seq (rewrite $X ($X \" US\"\
   )) concat))))\n      (unquote (index 1)))))\n  (rewrite (($A (@B))) ($A @B)))\n' <<\
   EOF\n(AMZN (foo bar) baz)\n(MSFT (foo bar))\nEOF\n```\n\nNotice how the inner part of \
   this command is the exact same `query`\nwe just did. But that's step 2 of a seque\
   nce of three steps, the first\nof which is to wrap the \"tail\" of our list in pare\
   ns, via:\n\n```sh\n(rewrite ($A @B) ($A (@B)))\n```\n\nand then, after we're done with\
  \ the meat of the rewrite, unwrap it\nagain via the inverse:\n\n```sh\n(rewrite (($A \
   (@B))) ($A @B))\n```\n\nwhere the extra parens around `(($A (@B)))` are added becau\
   se we're\ninside a `sexp change`.\n\nOverwriting all record values with a given key\
   \n----------------------------------------------\n\nSee the following little shell \
   command:\n\n```sh\nfunction proxy-iron-config-command {\n    no-args \"$@\"\n    host=j\
   ames\n    sexp change </etc/iron-config.sexp \\\n         \"(record (host (const $ho\
   st)))\"\n}\n```\n\nThis operates on a file with S-expressions of the form:\n\n```ocaml\n\
   ((host some_host_name)\n (async_rpc_port (Static 7013))\n (hgrc /path/to/hgrc)\n (s\
   erializer_pause_timeout 2m)\n (rpc_proxy_config\n  ((another_host1 (p1 p2 p3))\n   \
   (another_host2 (p1 p2)))))\n```\n\ni.e., records with keys `host`, `async_rpc_port`\
   , `hgrc`,\n`serializer_pause_timeout`, and so on. By writing\n`(record (host (cons\
   t $host)))`, the above sexp change program just\ntakes whatever the value of the \
   interpolated `$host` ends up being --\nthis is a shell variable, not a variable i\
   n the sexp change language\n-- and jams it into the place of _every_ `host` recor\
   d value. So if\n`$host` were \"foo\", the above record would be rewritten as:\n\n```o\
   caml\n((host foo)\n (async_rpc_port (Static 7013))\n (hgrc /path/to/hgrc)\n (seriali\
   zer_pause_timeout 2m)\n (rpc_proxy_config\n  ((another_host1 (p1 p2 p3))\n   (anoth\
   er_host2 (p1 p2)))))\n```\n\nand so on for the rest of the records in the file -- t\
   hey would all\nbegin `(host foo)`.\n\nDoing a bottom-up search with alt and try\n---\
   --------------------------------------\n\nHere, we use the `alt` keyword for the s\
   imple reason that it allows\nus to apply several different rewrite rules in paral\
   lel, in a single\npass through a file:\n\n```\nsexp change '(bottomup (try (alt\n    \
   (rewrite (App $X $Y) ($X $Y))\n    (rewrite (Name $X) $X)\n    (rewrite (Var (Free\
  \ $X)) $X)\n  )))'\n```\n\nThe `alt` keyword is short-circuiting, in the sense that i\
   t returns\nas soon as one of the rewrite rules you pass it succeeds. Here, we don\
   't\nreally take advantage of that short-circuiting: no matter what order\nwe put o\
   ur three rules in, the program will have the same behavior.\n\nIf, however, the LH\
   S of our alternative rewrite rules had the same\nstructure -- like if they all lo\
   oked like `(Foo $X) RHS` or `(Foo $X)\nRHS'` -- then there would at least be the \
   chance that the order of\nthe rules could matter.\n"
;;

let change_semantics_dot_md =
  "---\ntitle: Sexp change formal semantics\nparent: ../README.md\n---\n\nThis document \
   explains the semantics of change expressions, whose\nsyntax is as follows:\n\n     \
   C ::= (rewrite P P)\n         | (seq C C ...)\n         | (alt C C ...)\n         |\
  \ id\n         | fail\n         | delete\n         | (const SEXP)\n         | (try C)\
   \n         | (record (F C) ...)\n         | (children C)\n         | (topdown C)\n  \
  \       | (bottomup C)\n         | lowercase\n         | concat\n         | (query Q\
   )\n\n    F ::= ATOM\n\nThe meaning of an expression is a partial function on s-expre\
   ssions.\nThe toplevel command is applied to each input expression in the input.\n(\
   NOTE: this fragment of the language is essentially a fragment of Stratego\nterm r\
   ewriting language: http://strategoxt.org)\n\nRewriting\n=========\n\nA `(rewrite LHS \
   RHS)` attempts to rewrite an expression by the specified\nrewrite rule.  It first\
  \ matches the input s-expression against the\nLHS pattern.  If this succeeds, it i\
   nstantiates the RHS pattern with\nbindings inferred from the match.\n\nThe syntax o\
   f the LHS and RHS patterns is as follows:\n\n     P ::= ATOM | (P ... P) | $VAR | \
   @VAR\n\nNote that variables are distinguished from atoms by a prefix of `$` or `@`\
   .\nVariables starting with `$` are scalar and they match a single s-expression,\nw\
   hile those starting with `@` match a list of s-expressions.\n\nSome examples\n\n    \
  \         (rewrite foo bar) : foo => bar\n             (rewrite foo bar) : abc => \
   _|_          (indicates failure)\n             (rewrite foo bar) : (foo bar) => _\
   |_\n       (rewrite (foo bar) wow) : (foo bar) => wow\n         (rewrite (foo $X) \
   $X) : (foo bar) => bar\n         (rewrite (foo $X) $X) : (foo (bar none)) => (bar\
  \ none)\n    (rewrite (foo $X) ($X $X)) : (foo bar) => (bar bar)\n       (rewrite (\
   foo @X) (@X)) : (foo bar baz) => (bar baz)\n       (rewrite (foo @X) (@X)) : (foo\
  \ (bar a) (baz b)) => ((bar a) (baz b))\n    (rewrite (foo @X) (@X @X)) : (foo bar\
  \ baz) => (bar baz bar baz)\n\nSome rewrite rules are not well formed.  The followi\
   ng rules apply:\n\n    well-formedness rule                          non-conformin\
   g program\n    ------------------------------------------------------------------\
   -----\n    variables may not be bound more than once     (rewrite (foo $X $X) who\
   )\n    all RHS variables must be bound on LHS        (rewrite (foo bar) (yo $X))\n\
  \    no two list variables can be in the same list (rewrite (foo @X @Y) @X)\n\n`(co\
   nst SEXP)` is syntactic sugar for `(rewrite $_ SEXP)`.  That is, const is a\nrewr\
   ite rule that always succeeds and produces the same sexp.\n\n`(rewrite_record LHS \
   RHS)` should be used if order of s-expressions in lists\nshould be disregarded.  \
   Some examples\n\n    (rewrite_record (foo bar) wow) : (bar foo) => wow\n    (rewrit\
   e_record (foo bar) wow) : (foo bar) => wow\n    (rewrite_record (foo bar) wow) : \
   (foo) => _|_\n    (rewrite_record (foo bar) wow) : (bar) => _|_\n    (rewrite_reco\
   rd (bar @X) (wow @X)) : (foo bar baz) => (wow foo baz)\n\nComposition\n===========\n\
   \nThe two basic composition operators are `seq` and `alt`.  These two\noperators p\
   rovide sequencing and biased choice, respectively.  The\ndenotation of\n\n         \
  \           (seq C1 C2 ... Cn)\n\nis a composition of the denotations D = [C1] and \
   D' = [(seq C2 ... Cn)].\n\n    (seq C1 C2 ... Cn) : S => S''    if C1 : S => S'\n  \
  \                                 and (seq C2 ... Cn) : S' => S''\n\nFailure at any\
  \ point in the sequence propagates out to the whole.\n\n    (seq C1 C2 ... Cn) : S \
   => _|_    if C1 : S => _|_\n\n    (seq C1 C2 ... Cn) : S => _|_    if C1 : S => S'\
   \n                                   and (seq C2 ... Cn) : S' => _|_\n\nUnary appli\
   cation of seq has no effect\n\n    (seq C) = C\n\nThe denotation of\n\n               \
  \     (alt C1 C2 ... Cn)\n\nis simply a biased choice of whichever of C1 ... Cn fir\
   st succeeds on\nthe input.\n\n    (alt C1 C2 ... Cn) : S => S'   if C1 : S => S'\n\n \
  \   (alt C1 C2 ... Cn) : S => R    if C1 : S => _|_ and (alt C2 ... Cn) : S => R\n\
  \                                 (where R is the either _|_ or an s-expression)\n\
   \nThere are two trivial expressions, `id` and `fail`.  A id expression\nsimply sel\
   ects it input, while a `fail` expression selects nothing\n\n                id : S\
  \ => S           fail : S => _|_\n\nThese are identities for the seq and alt operat\
   ors.\n\n                id = (seq)            fail = (alt)\n\nOne other pattern is s\
   o common that it is part of the language itself.\n\n                (try C) = (alt\
  \ C id)\n\nRecords\n=======\n\nThe `record` syntax is used to rewrite record sexps of \
   the form\n((field1 value1) ... (fieldn value_n)).  With `record`, one specifies f\
   or\neach field name how to change the value of the field with that name, and\nopti\
   onally what to do with fields not explicitly mentioned.  For example:\n\n    (reco\
   rd (a1 delete) (a2 (const 13)) (a3 (rewrite $X ($X $X))))\n    :  ((a1 v1) (a2 v2\
   ) (a3 v3))\n    => (        (a2 13) (a3 (v3 v3)))\n\nBy default, a record rewrite o\
   nly succeeds if all of the fields appear in the\ninput.  Also by default, fields \
   that appear in the input that are not mentioned\nin the record are preserved.\n\nOn\
   e can allow the rewrite to succeed even if a field is missing by adding an\nattri\
   bute.\n\n    (record (f1            delete)) : ((f2 v2)) => _|_\n    (record (f1 (o\
   ptional) delete)) : ((f2 v2)) => ((f2 v2))\n\nFields that are in the record but no\
   t in the input are treated as if their\nvalue is ().  This makes it possible to a\
   dd fields.  For example:\n\n    (record (a1 (optional) id         )) : () -> ((a1 \
   ()))\n    (record (a1 (optional) (const foo))) : () -> ((a1 foo))\n\nOne can use th\
   e special `_` as a field name to explicitly specify what to\ndo with fields in th\
   e input that aren't mentioned in the record.\n\n    (record (a1 (const 13)) (_ id)\
   )    # the default, same as not using _\n    :   ((a1 v1) (a2 v2))\n    ==> ((a1 1\
   3) (a2 v2))\n\n    (record (a1 id) (_ delete))        # delete unmentioned fields\n\
  \    :   ((a1 v1) (a2 v2) (a3 v3))\n    ==> ((a1 v1))\n\n    (record (a1 id) (_ fail\
   ))         # fail if there are unmentioned fields\n    :   ((a1 v1) (a2 v2)\n    =\
   => _|_\n\nOne can optionally specify a new field name, which will cause the field\n\
   name to be changed.\n\n    (record (a1 ((rename a2)) id)) : ((a1 13)) -> ((a2 13))\
   \n\nThere are a couple of well-formedness rules on record syntax.\n\n    well-formed\
   ness rule                          non-conforming program\n    ------------------\
   -----------------------------------------------------------\n    a field name can\
  \ occur at most once           (record (foo id) (foo delete))\n    if there an _ f\
   ield, it must occur last       (record (_ delete) (foo id))\n\nTraversals\n========\
   ==\n\nThe expression `(children C)` transforms all the immediate\nsub-expressions o\
   f a s-expression according to C and returns the result.\n\n          (children (re\
   write foo bar)) : (foo foo) => (bar bar)\n\nIf C fails on any of the children, the\
  \ failure propagates outward.\n\n          (children (rewrite foo bar)) : (foo wow)\
  \ => _|_\n\nThis is easily overcome, if desired\n\n    (children (try (rewrite foo ba\
   r))) : (foo wow) => (bar wow)\n\nIf the input is atomic (i.e., there are no childr\
   en), then C is trivially\nsuccessful on *all* the children.\n\n          (children \
   (rewrite foo bar)) : wow => wow\n\nTop-down and bottom-up iterated traversal strat\
   egies are defined recursively\nin terms of children expressions.\n\n    (topdown C)\
  \ = (seq C (children (topdown C)))\n\n    (bottomup C) = (seq (children (bottomup C\
   )) C)\n\nFor example:\n\n     (topdown (try (rewrite a b))) : (a (c a)) => (b (c b))\
   \n    (bottomup (try (rewrite a b))) : (a (c a)) => (b (c b))\n\nThe difference bet\
   ween topdown and bottomup can be seen here:\n\n    if\n\n    C = (try (rewrite (not \
   (and $A $B)) (or (not $A) (not $B))))\n\n    then\n\n    (topdown C)\n      : (not (a\
   nd a (and b c))) => (or (not a) (or (not b) (not c)))\n\n    but\n\n    (bottomup C)\
   \n      : (not (and a (and b c))) => (or (not a) (not (and b c)))\n\nNote that one \
   must take care when using topdown to avoid infinite loops!\n\n    (topdown (rewrit\
   e a (a a))) : a => ...      (never returns!)\n\nDeletion\n========\n\n`delete` allows\
  \ one to delete a sexp from its containing sexp.  Semantically,\nit is essentially\
  \ a special kind of return value that is recognized by other\nconstructs, like `ch\
   ildren`, and causes them to delete the component sexp.\n\n    delete : foo => _|_\n\
  \     (children delete) : (foo bar) => ()\n     (children (alt (rewrite foo 13) de\
   lete)) : (foo bar) => (13)\n\n`delete` satisfies the following equalities\n\n    (se\
   q delete C) == delete\n    (alt delete C) == delete\n\nMiscellaneous\n=============\n\
   \n`lowercase` does exactly what you would think it does.\n\n    lowercase : Word   \
  \     => word\n    lowercase : UPPERCASE   => uppercase\n    lowercase : CamelCase \
  \  => camelcase\n    lowercase : (A (B C) D) => (a (b c) d)\n    lowercase : 1234  \
  \      => 1234\n\n`concat` concatenates all the atoms of the input expression into \
   a single\natom.\n\n    concat : Word          => Word\n    concat : (' \"A B\" ') => \"\
   'A B'\"\n    concat : (A (B C) D)   => ABCD\n\nSub-queries\n===========\n\nFinally, we \
   have a way to call the query language from the change\nlanguage.  The semantics o\
   f `(query Q)` are to gather up all the\ns-expressions output by running the query\
  \ Q against the input and\ngathering them all up into a single list.\n\n    (query Q\
   ) : S => (X1 ... Xn)    if Q : S => {X1, ... , Xn}\n\n(see the query subcommand's \
   internal documentation for information on\nquery semantics).\n\nNote that this oper\
   ation always succeeds (never results in _|_).\n"
;;

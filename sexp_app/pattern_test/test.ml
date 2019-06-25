open Core
open Expect_test_helpers_kernel
open Sexp_app_pattern

let show_parse querystr =
  let query = Parser.parse_exn querystr in
  printf "%s : %s\n" querystr (Sexp.to_string (Query.sexp_of_t query))
;;

let show_parse_fail querystr =
  printf "%s : " querystr;
  show_raise ~hide_positions:true (fun () -> Parser.parse_exn querystr)
;;

let run ?output querystr sexpstrs =
  let output = Option.map output ~f:(fun s -> Output.t_of_sexp (Sexp.of_string s)) in
  let query = Parser.parse_exn querystr in
  List.iter sexpstrs ~f:(fun sexpstr ->
    let sexp = Sexp.of_string sexpstr in
    printf ":";
    Engine.iter_matches ~query ~output sexp ~wrap_singletons:false ~f:(fun s ->
      printf !"%{Sexp} " s);
    printf "\n")
;;

let run_single ?output ?(wrap_singletons = false) querystr sexpstr =
  let output = Option.map output ~f:(fun s -> Output.t_of_sexp (Sexp.of_string s)) in
  let query = Parser.parse_exn querystr in
  let sexp = Sexp.of_string sexpstr in
  printf ":";
  Engine.iter_matches ~query ~output sexp ~wrap_singletons ~f:(fun s ->
    printf !"%{Sexp}\n" s)
;;

let replace_single querystr ~replace ~with_ sexpstr =
  let with_ = Output.t_of_sexp (Sexp.of_string with_) in
  let query = Parser.parse_exn querystr in
  let sexp = Sexp.of_string sexpstr in
  printf ":";
  let result = Engine.replace ~query ~replace ~with_ sexp ~wrap_singletons:false in
  printf !"%{Sexp}\n" result
;;

let%expect_test _ =
  show_parse ".";
  show_parse ". .";
  show_parse "\n. .\t  ";
  show_parse "\".\"";
  show_parse "\")\"";
  show_parse "\")(##$*&)#!*&^#@\"";
  show_parse "a\"b\"";
  show_parse "\"\\\"\"";
  show_parse "\"%\"";
  show_parse_fail "\"";
  show_parse_fail "\"ab";
  show_parse_fail "\"ab\"\"";
  show_parse "a";
  show_parse "abc";
  show_parse " abc ";
  show_parse "a bc";
  show_parse ".c";
  show_parse "c.";
  show_parse "c .";
  show_parse ". c";
  show_parse "a.bc";
  show_parse "[]";
  show_parse "[][]";
  show_parse "[][][]";
  show_parse "[[]]";
  show_parse "a[c]";
  show_parse "[a[c]]";
  show_parse "[a]c";
  show_parse "[ac]";
  show_parse_fail "[ac";
  show_parse_fail "a]c";
  show_parse "/.*/";
  show_parse "/^[abc]?/";
  show_parse "/\\//";
  show_parse "/\\\\abc/";
  show_parse_fail "/\\\\//";
  show_parse_fail "/";
  show_parse "123";
  show_parse "123?";
  show_parse "1 23*";
  show_parse "123*";
  show_parse "123*?";
  show_parse "123?*";
  show_parse "123*a**b";
  show_parse_fail "abc *";
  show_parse_fail "abc ?";
  show_parse_fail "*";
  show_parse_fail "**";
  show_parse_fail "?abc";
  show_parse "abc .*";
  show_parse "abc.*";
  show_parse "[a*]";
  show_parse_fail "a[*]";
  show_parse "()";
  show_parse "(a)";
  show_parse "(a b)";
  show_parse "( ab)";
  show_parse "(ab*)";
  show_parse "(ab* )";
  show_parse "(ab )*";
  show_parse_fail "(ab ) *";
  show_parse "( a?b)?";
  show_parse "()*";
  show_parse "(())";
  show_parse "(()(()))";
  show_parse "(()[()][])";
  show_parse ".(.)";
  show_parse_fail "(ab[)]";
  show_parse "{}";
  show_parse "{a}";
  show_parse "{a b}";
  show_parse "{[a b]}";
  show_parse "{a [b c] d}";
  show_parse "{a ()}";
  show_parse "{a b*}";
  show_parse "{ab b. . (c)}";
  show_parse "{ab {} b. {{c}}}";
  show_parse_fail "{a b *}";
  show_parse_fail "{*}";
  show_parse "%1";
  show_parse "%012";
  show_parse "%-2";
  show_parse "%abc";
  show_parse "%a bc";
  show_parse "%a%bc";
  show_parse_fail "%";
  show_parse_fail "%%";
  show_parse_fail "%%a";
  show_parse_fail "% a";
  show_parse "%.";
  show_parse "%.a";
  show_parse_fail "% .";
  show_parse_fail "%..";
  show_parse "a%1";
  show_parse "%1*";
  show_parse "%a*";
  show_parse "%[a]*";
  show_parse "%a+";
  show_parse "%[a]+";
  show_parse "%a?";
  show_parse "%()";
  show_parse_fail "%%()";
  show_parse "%(%a)";
  show_parse "%[%a]";
  show_parse "%{%.}";
  show_parse "%/abc/";
  show_parse ".. a";
  show_parse_fail "..";
  show_parse ".. ()";
  show_parse " (.. a)";
  show_parse "a .. b";
  show_parse "a [.. b] ";
  show_parse_fail "[a ..] b";
  show_parse "(... a)";
  show_parse_fail "(..)";
  show_parse_fail "a [..] b";
  show_parse "(a .. b .. c)";
  show_parse "([a .. b] .. c )";
  show_parse "{ a .. b }";
  show_parse "{a .. b .. c}";
  show_parse "a .. %b";
  show_parse "a .. b*";
  show_parse "%a? .. b";
  show_parse "%[a .. b]";
  show_parse "a & b";
  show_parse_fail "a && b";
  show_parse "a | b";
  show_parse "a & b";
  show_parse ".. a & b";
  show_parse ".. a | b";
  show_parse "[.. a & b]";
  show_parse "[.. a | b]";
  show_parse ".. [a & b]";
  show_parse ".. [a | b]";
  show_parse_fail "a && b";
  show_parse "(a b & c d)";
  show_parse "(a & b & c | d)";
  show_parse "(a b & c | d & e f)";
  show_parse "(a & e .. f & g)";
  show_parse "(a &e .. f& g)";
  show_parse_fail "{a | b}";
  show_parse "{[a | b]}";
  show_parse "%a=b&c";
  show_parse "%a=()";
  show_parse_fail "%a = b";
  show_parse_fail "%()=b";
  show_parse "%a=b*";
  show_parse "%abc=()*";
  show_parse "%1=()*";
  show_parse "%1=a..b";
  show_parse "%1=a .. b";
  show_parse_fail "%a=.. b";
  show_parse "%a=[.. b]";
  show_parse_fail "%a=*";
  show_parse "%\"a=*\"";
  show_parse "( %a=b c d )";
  show_parse "{ %a=b c d }";
  show_parse "!(a)";
  show_parse "!{a}";
  show_parse "![a]";
  show_parse_fail "!a";
  show_parse_fail "!.. a";
  show_parse "a !()!()";
  [%expect
    {|
    . : Any
    . . : (Sequence(Any Any))

    . .	   : (Sequence(Any Any))
    "." : (Atom .)
    ")" : (Atom")")
    ")(##$*&)#!*&^#@" : (Atom")(##$*&)#!*&^#@")
    a"b" : (Sequence((Atom a)(Atom b)))
    "\"" : (Atom"\"")
    "%" : (Atom %)
    " : (raised (
      exn.ml.Reraised
      "Parsing match query failed at line 1 char 1 in query \""
      (Failure "Match.Lexer.scan_string: unterminated string at line 1 char 0")))
    "ab : (raised (
      exn.ml.Reraised
      "Parsing match query failed at line 1 char 3 in query \"ab"
      (Failure "Match.Lexer.scan_string: unterminated string at line 1 char 0")))
    "ab"" : (raised (
      exn.ml.Reraised
      "Parsing match query failed at line 1 char 5 in query \"ab\"\""
      (Failure "Match.Lexer.scan_string: unterminated string at line 1 char 4")))
    a : (Atom a)
    abc : (Atom abc)
     abc  : (Atom abc)
    a bc : (Sequence((Atom a)(Atom bc)))
    .c : (Atom .c)
    c. : (Atom c.)
    c . : (Sequence((Atom c)Any))
    . c : (Sequence(Any(Atom c)))
    a.bc : (Atom a.bc)
    [] : (Sequence())
    [][] : (Sequence((Sequence())))
    [][][] : (Sequence((Sequence())(Sequence())))
    [[]] : (Sequence())
    a[c] : (Sequence((Atom a)(Atom c)))
    [a[c]] : (Sequence((Atom a)(Atom c)))
    [a]c : (Sequence((Atom a)(Atom c)))
    [ac] : (Atom ac)
    [ac : (raised (Failure "Parsing match query failed at line 1 char 3 in query [ac"))
    a]c : (raised (Failure "Parsing match query failed at line 1 char 1 in query a]c"))
    /.*/ : (Atom_regex .*)
    /^[abc]?/ : (Atom_regex ^[abc]?)
    /\// : (Atom_regex /)
    /\\abc/ : (Atom_regex"\\abc")
    /\\// : (raised (
      exn.ml.Reraised
      "Parsing match query failed at line 1 char 5 in query /\\\\//"
      (Failure "Error parsing unterminated regex at line 1 char 4")))
    / : (raised (
      exn.ml.Reraised
      "Parsing match query failed at line 1 char 1 in query /"
      (Failure "Error parsing unterminated regex at line 1 char 0")))
    123 : (Atom 123)
    123? : (Maybe(Atom 123))
    1 23* : (Sequence((Atom 1)(Star(Atom 23))))
    123* : (Star(Atom 123))
    123*? : (Maybe(Star(Atom 123)))
    123?* : (Star(Maybe(Atom 123)))
    123*a**b : (Sequence((Star(Atom 123))(Star(Star(Atom a)))(Atom b)))
    abc * : (raised (Failure "Parsing match query failed at line 1 char 4 in query abc *"))
    abc ? : (raised (Failure "Parsing match query failed at line 1 char 4 in query abc ?"))
    * : (raised (Failure "Parsing match query failed at line 1 char 0 in query *"))
    ** : (raised (Failure "Parsing match query failed at line 1 char 0 in query **"))
    ?abc : (raised (Failure "Parsing match query failed at line 1 char 0 in query ?abc"))
    abc .* : (Sequence((Atom abc)(Star Any)))
    abc.* : (Star(Atom abc.))
    [a*] : (Star(Atom a))
    a[*] : (raised (Failure "Parsing match query failed at line 1 char 2 in query a[*]"))
    () : (List(Sequence()))
    (a) : (List(Atom a))
    (a b) : (List(Sequence((Atom a)(Atom b))))
    ( ab) : (List(Atom ab))
    (ab*) : (List(Star(Atom ab)))
    (ab* ) : (List(Star(Atom ab)))
    (ab )* : (Star(List(Atom ab)))
    (ab ) * : (raised (
      Failure "Parsing match query failed at line 1 char 6 in query (ab ) *"))
    ( a?b)? : (Maybe(List(Sequence((Maybe(Atom a))(Atom b)))))
    ()* : (Star(List(Sequence())))
    (()) : (List(List(Sequence())))
    (()(())) : (List(Sequence((List(Sequence()))(List(List(Sequence()))))))
    (()[()][]) : (List(Sequence((List(Sequence()))(List(Sequence())))))
    .(.) : (Sequence(Any(List Any)))
    (ab[)] : (raised (
      Failure "Parsing match query failed at line 1 char 4 in query (ab[)]"))
    {} : (Set())
    {a} : (Set((Atom a)))
    {a b} : (Set((Atom a)(Atom b)))
    {[a b]} : (Set((Sequence((Atom a)(Atom b)))))
    {a [b c] d} : (Set((Atom a)(Sequence((Atom b)(Atom c)))(Atom d)))
    {a ()} : (Set((Atom a)(List(Sequence()))))
    {a b*} : (Set((Atom a)(Star(Atom b))))
    {ab b. . (c)} : (Set((Atom ab)(Atom b.)Any(List(Atom c))))
    {ab {} b. {{c}}} : (Set((Atom ab)(Set())(Atom b.)(Set((Set((Atom c)))))))
    {a b *} : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query {a b *}"))
    {*} : (raised (Failure "Parsing match query failed at line 1 char 1 in query {*}"))
    %1 : (Capture_to_number 1 Any)
    %012 : (Capture_to_number 12 Any)
    %-2 : (Capture_to_field -2 Any)
    %abc : (Capture_to_field abc Any)
    %a bc : (Sequence((Capture_to_field a Any)(Atom bc)))
    %a%bc : (Sequence((Capture_to_field a Any)(Capture_to_field bc Any)))
    % : (raised (Failure "Parsing match query failed at line 1 char 1 in query %"))
    %% : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%"))
    %%a : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%a"))
    % a : (raised (Failure "Parsing match query failed at line 1 char 1 in query % a"))
    %. : (Capture_unlabeled Any)
    %.a : (Capture_to_field .a Any)
    % . : (raised (Failure "Parsing match query failed at line 1 char 1 in query % ."))
    %.. : (raised (Failure "Parsing match query failed at line 1 char 1 in query %.."))
    a%1 : (Sequence((Atom a)(Capture_to_number 1 Any)))
    %1* : (Star(Capture_to_number 1 Any))
    %a* : (Star(Capture_to_field a Any))
    %[a]* : (Star(Capture_unlabeled(Atom a)))
    %a+ : (Plus(Capture_to_field a Any))
    %[a]+ : (Plus(Capture_unlabeled(Atom a)))
    %a? : (Maybe(Capture_to_field a Any))
    %() : (Capture_unlabeled(List(Sequence())))
    %%() : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%()"))
    %(%a) : (Capture_unlabeled(List(Capture_to_field a Any)))
    %[%a] : (Capture_unlabeled(Capture_to_field a Any))
    %{%.} : (Capture_unlabeled(Set((Capture_unlabeled Any))))
    %/abc/ : (Capture_unlabeled(Atom_regex abc))
    .. a : (Subsearch(Atom a))
    .. : (raised (Failure "Parsing match query failed at line 1 char 2 in query .."))
    .. () : (Subsearch(List(Sequence())))
     (.. a) : (List(Subsearch(Atom a)))
    a .. b : (Sequence((Atom a)(Subsearch(Atom b))))
    a [.. b]  : (Sequence((Atom a)(Subsearch(Atom b))))
    [a ..] b : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query [a ..] b"))
    (... a) : (List(Sequence((Atom ...)(Atom a))))
    (..) : (raised (Failure "Parsing match query failed at line 1 char 3 in query (..)"))
    a [..] b : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query a [..] b"))
    (a .. b .. c) : (List(Sequence((Atom a)(Subsearch(Sequence((Atom b)(Subsearch(Atom c))))))))
    ([a .. b] .. c ) : (List(Sequence((Sequence((Atom a)(Subsearch(Atom b))))(Subsearch(Atom c)))))
    { a .. b } : (Set((Atom a)(Subsearch(Atom b))))
    {a .. b .. c} : (Set((Atom a)(Subsearch(Sequence((Atom b)(Subsearch(Atom c)))))))
    a .. %b : (Sequence((Atom a)(Subsearch(Capture_to_field b Any))))
    a .. b* : (Sequence((Atom a)(Subsearch(Star(Atom b)))))
    %a? .. b : (Sequence((Maybe(Capture_to_field a Any))(Subsearch(Atom b))))
    %[a .. b] : (Capture_unlabeled(Sequence((Atom a)(Subsearch(Atom b)))))
    a & b : (And((Atom a)(Atom b)))
    a && b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query a && b"))
    a | b : (Or_all((Atom a)(Atom b)))
    a & b : (And((Atom a)(Atom b)))
    .. a & b : (And((Subsearch(Atom a))(Atom b)))
    .. a | b : (Or_all((Subsearch(Atom a))(Atom b)))
    [.. a & b] : (And((Subsearch(Atom a))(Atom b)))
    [.. a | b] : (Or_all((Subsearch(Atom a))(Atom b)))
    .. [a & b] : (Subsearch(And((Atom a)(Atom b))))
    .. [a | b] : (Subsearch(Or_all((Atom a)(Atom b))))
    a && b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query a && b"))
    (a b & c d) : (List(And((Sequence((Atom a)(Atom b)))(Sequence((Atom c)(Atom d))))))
    (a & b & c | d) : (List(Or_all((And((Atom a)(Atom b)(Atom c)))(Atom d))))
    (a b & c | d & e f) : (List(Or_all((And((Sequence((Atom a)(Atom b)))(Atom c)))(And((Atom d)(Sequence((Atom e)(Atom f))))))))
    (a & e .. f & g) : (List(And((Atom a)(Sequence((Atom e)(Subsearch(Atom f))))(Atom g))))
    (a &e .. f& g) : (List(And((Atom a)(Sequence((Atom e)(Subsearch(Atom f))))(Atom g))))
    {a | b} : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query {a | b}"))
    {[a | b]} : (Set((Or_all((Atom a)(Atom b)))))
    %a=b&c : (And((Capture_to_field a(Atom b))(Atom c)))
    %a=() : (Capture_to_field a(List(Sequence())))
    %a = b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query %a = b"))
    %()=b : (raised (Failure "Parsing match query failed at line 1 char 3 in query %()=b"))
    %a=b* : (Star(Capture_to_field a(Atom b)))
    %abc=()* : (Star(Capture_to_field abc(List(Sequence()))))
    %1=()* : (Star(Capture_to_number 1(List(Sequence()))))
    %1=a..b : (Capture_to_number 1(Atom a..b))
    %1=a .. b : (Sequence((Capture_to_number 1(Atom a))(Subsearch(Atom b))))
    %a=.. b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query %a=.. b"))
    %a=[.. b] : (Capture_to_field a(Subsearch(Atom b)))
    %a=* : (raised (Failure "Parsing match query failed at line 1 char 3 in query %a=*"))
    %"a=*" : (Capture_unlabeled(Atom a=*))
    ( %a=b c d ) : (List(Sequence((Capture_to_field a(Atom b))(Atom c)(Atom d))))
    { %a=b c d } : (Set((Capture_to_field a(Atom b))(Atom c)(Atom d)))
    !(a) : (First_match_only(List(Atom a)))
    !{a} : (First_match_only(Set((Atom a))))
    ![a] : (First_match_only(Atom a))
    !a : (raised (Failure "Parsing match query failed at line 1 char 1 in query !a"))
    !.. a : (raised (Failure "Parsing match query failed at line 1 char 1 in query !.. a"))
    a !()!() : (Sequence((Atom a)(First_match_only(List(Sequence())))(First_match_only(List(Sequence()))))) |}]
;;

let standard_test_cases =
  [ "abc"
  ; "()"
  ; "(())"
  ; "(abc)"
  ; "((abc))"
  ; "(a b c)"
  ; "(a (b) c)"
  ; "((a 1)(b 2)(c 3))"
  ; "((a 1 1)(b 2 2)(c 3 3))"
  ; "( ((a 1)(b 2)(c 3)) ((d 3)(e 4)(f 5)) )"
  ]
;;

let%expect_test _ =
  run "%." standard_test_cases;
  [%expect
    {|
    :abc
    :()
    :(())
    :(abc)
    :((abc))
    :(a b c)
    :(a(b)c)
    :((a 1)(b 2)(c 3))
    :((a 1 1)(b 2 2)(c 3 3))
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) |}]
;;

let%expect_test _ =
  run "%[.*]" standard_test_cases;
  [%expect
    {|
    :abc
    :()
    :(())
    :(abc)
    :((abc))
    :(a b c)
    :(a(b)c)
    :((a 1)(b 2)(c 3))
    :((a 1 1)(b 2 2)(c 3 3))
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) |}]
;;

let%expect_test _ =
  run "%(.)" standard_test_cases;
  [%expect
    {|
    :
    :
    :(())
    :(abc)
    :((abc))
    :
    :
    :
    :
    : |}]
;;

let%expect_test _ =
  run "(%.)" standard_test_cases;
  [%expect {|
    :
    :
    :()
    :abc
    :(abc)
    :
    :
    :
    :
    : |}]
;;

let%expect_test _ =
  run "%(. . .)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :(a b c)
    :(a(b)c)
    :((a 1)(b 2)(c 3))
    :((a 1 1)(b 2 2)(c 3 3))
    : |}]
;;

let%expect_test _ =
  run "{%.}" standard_test_cases;
  [%expect
    {|
    :
    :
    :()
    :abc
    :(abc)
    :a b c
    :a (b) c
    :(a 1) (b 2) (c 3)
    :(a 1 1) (b 2 2) (c 3 3)
    :((a 1)(b 2)(c 3)) ((d 3)(e 4)(f 5)) |}]
;;

let%expect_test _ =
  run "%0" standard_test_cases;
  [%expect
    {|
    :(abc)
    :(())
    :((()))
    :((abc))
    :(((abc)))
    :((a b c))
    :((a(b)c))
    :(((a 1)(b 2)(c 3)))
    :(((a 1 1)(b 2 2)(c 3 3)))
    :((((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))) |}]
;;

let%expect_test _ =
  run "%field" standard_test_cases;
  [%expect
    {|
    :((field abc))
    :((field()))
    :((field(())))
    :((field(abc)))
    :((field((abc))))
    :((field(a b c)))
    :((field(a(b)c)))
    :((field((a 1)(b 2)(c 3))))
    :((field((a 1 1)(b 2 2)(c 3 3))))
    :((field(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))))) |}]
;;

let%expect_test _ =
  run "(%0)" standard_test_cases;
  [%expect
    {|
    :
    :
    :(())
    :(abc)
    :((abc))
    :
    :
    :
    :
    : |}]
;;

let%expect_test _ =
  run "{%0}" standard_test_cases;
  [%expect
    {|
    :
    :
    :(())
    :(abc)
    :((abc))
    :(a) (b) (c)
    :(a) ((b)) (c)
    :((a 1)) ((b 2)) ((c 3))
    :((a 1 1)) ((b 2 2)) ((c 3 3))
    :(((a 1)(b 2)(c 3))) (((d 3)(e 4)(f 5))) |}]
;;

let%expect_test _ =
  run ".. (%foo %bar %baz)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :((foo a)(bar b)(baz c))
    :((foo a)(bar(b))(baz c))
    :((foo(a 1))(bar(b 2))(baz(c 3)))
    :((foo(a 1 1))(bar(b 2 2))(baz(c 3 3))) ((foo a)(bar 1)(baz 1)) ((foo b)(bar 2)(baz 2)) ((foo c)(bar 3)(baz 3))
    :((foo(a 1))(bar(b 2))(baz(c 3))) ((foo(d 3))(bar(e 4))(baz(f 5))) |}]
;;

let%expect_test _ =
  run ~output:"(%foo ((%baz)) (barr (%bar)))" ".. (%foo %bar %baz)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :(a((c))(barr(b)))
    :(a((c))(barr((b))))
    :((a 1)(((c 3)))(barr((b 2))))
    :((a 1 1)(((c 3 3)))(barr((b 2 2)))) (a((1))(barr(1))) (b((2))(barr(2))) (c((3))(barr(3)))
    :((a 1)(((c 3)))(barr((b 2)))) ((d 3)(((f 5)))(barr((e 4)))) |}]
;;

let%expect_test _ =
  run "%(.+)" standard_test_cases;
  [%expect
    {|
    :
    :
    :(())
    :(abc)
    :((abc))
    :(a b c)
    :(a(b)c)
    :((a 1)(b 2)(c 3))
    :((a 1 1)(b 2 2)(c 3 3))
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) |}]
;;

let%expect_test _ =
  run "(.* %. .*)" standard_test_cases;
  [%expect
    {|
    :
    :
    :()
    :abc
    :(abc)
    :a b c
    :a (b) c
    :(a 1) (b 2) (c 3)
    :(a 1 1) (b 2 2) (c 3 3)
    :((a 1)(b 2)(c 3)) ((d 3)(e 4)(f 5)) |}]
;;

let%expect_test _ =
  run "(.* %.)" standard_test_cases;
  [%expect
    {|
    :
    :
    :()
    :abc
    :(abc)
    :c
    :c
    :(c 3)
    :(c 3 3)
    :((d 3)(e 4)(f 5)) |}]
;;

let%expect_test _ =
  run ".. b %." standard_test_cases;
  [%expect {|
    :
    :
    :
    :
    :
    :c
    :
    :2
    :2
    :2 |}]
;;

let%expect_test _ =
  run ".. %{b %.}" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :((a b c)a) ((a b c)b) ((a b c)c)
    :((b)b)
    :((b 2)b) ((b 2)2)
    :((b 2 2)b) ((b 2 2)2) ((b 2 2)2)
    :((b 2)b) ((b 2)2) |}]
;;

let%expect_test _ =
  run ".. (.? %. 3)" standard_test_cases;
  [%expect {|
    :
    :
    :
    :
    :
    :
    :
    :c
    :3
    :c d |}]
;;

let%expect_test _ =
  run ".. %(. .)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :
    :
    :(a 1) (b 2) (c 3)
    :
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) (a 1) (b 2) (c 3) (d 3) (e 4) (f 5) |}]
;;

let%expect_test _ =
  run "![.. %(. .)]" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :
    :
    :(a 1)
    :
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) |}]
;;

let%expect_test _ =
  run ".. . %. %.?" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :(b()) (b c) (c())
    :((b)()) ((b)c) (c())
    :((b 2)()) ((b 2)(c 3)) (1()) ((c 3)()) (2()) (3())
    :((b 2 2)()) ((b 2 2)(c 3 3)) (1()) (1 1) (1()) ((c 3 3)()) (2()) (2 2) (2()) (3()) (3 3) (3())
    :(((d 3)(e 4)(f 5))()) ((b 2)()) ((b 2)(c 3)) (1()) ((c 3)()) (2()) (3()) ((e 4)()) ((e 4)(f 5)) (3()) ((f 5)()) (4()) (5()) |}]
;;

let%expect_test _ =
  run ".. (. %. %.?)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :(b c)
    :((b)c)
    :((b 2)(c 3)) (1()) (2()) (3())
    :((b 2 2)(c 3 3)) (1 1) (2 2) (3 3)
    :(((d 3)(e 4)(f 5))()) ((b 2)(c 3)) (1()) (2()) (3()) ((e 4)(f 5)) (3()) (4()) (5()) |}]
;;

let%expect_test _ =
  run ".. (a %.) & .. (c %.) " standard_test_cases;
  [%expect {|
    :
    :
    :
    :
    :
    :
    :
    :(1 3)
    :
    :(1 3) |}]
;;

let%expect_test _ =
  run ".. ([a|d] %.) & .. ([c|e] %.)" standard_test_cases;
  [%expect
    {|
      :
      :
      :
      :
      :
      :
      :
      :(1 3)
      :
      :(1 3) (1 4) (3 3) (3 4)
|}]
;;

let%expect_test _ =
  run "{[ .. ([a|d] %.) & .. ([c|e] %.) ]}" standard_test_cases;
  [%expect
    {|
      :
      :
      :
      :
      :
      :
      :
      :
      :
      :(1 3) (3 4)
|}]
;;

let%expect_test _ =
  run ".. {%(a .) %(b .)}" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :
    :
    :((a 1)(b 2))
    :
    :((a 1)(b 2)) |}]
;;

let%expect_test _ =
  run ".. [ {%(a .)} & {%(b .)} ]" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :
    :
    :((a 1)(b 2))
    :
    :((a 1)(b 2)) |}]
;;

let%expect_test _ =
  run "[%[[([a])]]]" [ "a"; "(a)"; "()"; "(a a)" ];
  [%expect {|
    :
    :(a)
    :
    : |}]
;;

let%expect_test _ =
  run_single "(%a %1)" "(c d)";
  [%expect {|
    :((a c)(1 d)) |}]
;;

let%expect_test _ =
  run_single "(%1 %0)" "(c d)";
  [%expect {|
    :(d c) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "." "(c d)");
  [%expect {|
    :(raised (Failure "No captures % were specified in pattern")) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%2 %1)" "(c d)");
  [%expect
    {|
    :(raised (
      Failure
      "Match pattern uses captures up to %2 but is missing %0 (reminder: numbered captures should be zero-indexed)")) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%2 %0)" "(c d)");
  [%expect
    {|
    :(raised (Failure "Match pattern uses captures up to %2 but is missing %1")) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%0 %.)" "(c d)");
  [%expect
    {|
    :(raised (
      Failure
      "Cannot mix unlabeled captures with named or numbered captures in the same pattern")) |}]
;;

let%expect_test _ =
  run_single ".. (foo (.* %. .*))" "((foo (A B C)) (bar (D E F)) (foo (foo (G H I))))";
  [%expect {|
    :A
    B
    C
    foo
    (G H I)
    G
    H
    I |}]
;;

let%expect_test _ =
  run_single ".. (foo (%[.*]))" "((foo (A B C)) (bar (D E F)) (foo (foo (G H I))))";
  [%expect {|
    :(A B C)
    (foo(G H I))
    (G H I) |}]
;;

let%expect_test _ =
  run_single "(.* %[foo]+ .*)" "(foo foo bar baz foo bar)";
  (* yes, we get FOUR of them. The extra foo comes from when %[foo]+ matches
     "foo foo", and since the capture is inside the +, it successively captures
     the first "foo" and then second "foo" which overwrites the first. *)
  [%expect {|
    :foo
    foo
    foo
    foo |}]
;;

let%expect_test _ =
  run_single "(.* %[foo+] .*)" "(foo foo bar baz foo bar)";
  [%expect {|
    :foo
    (foo foo)
    foo
    foo |}]
;;

let%expect_test _ =
  run_single ~wrap_singletons:true "(.* %[foo+] .*)" "(foo foo bar baz foo bar)";
  [%expect {|
    :(foo)
    (foo foo)
    (foo)
    (foo) |}]
;;

let%expect_test _ =
  run_single ~wrap_singletons:false "(.* %[foo+] .* %.)" "(foo foo bar baz foo bar)";
  [%expect {|
    :(foo bar)
    ((foo foo)bar)
    (foo bar)
    (foo bar) |}]
;;

let%expect_test _ =
  run_single ~wrap_singletons:true "(.* %[foo+] .* %.)" "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((foo)(bar))
    ((foo foo)(bar))
    ((foo)(bar))
    ((foo)(bar)) |}]
;;

let%expect_test _ =
  run_single "(.* %a=foo+ .*)" "(foo foo bar baz foo bar)";
  [%expect {|
    :((a foo))
    ((a foo))
    ((a foo))
    ((a foo)) |}]
;;

let%expect_test _ =
  run_single "(.* %a=[foo+] .*)" "(foo foo bar baz foo bar)";
  [%expect {|
    :((a foo))
    ((a(foo foo)))
    ((a foo))
    ((a foo)) |}]
;;

let%expect_test _ =
  run_single "(.* %a=foo* .*)" "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((a()))
    ((a foo))
    ((a foo))
    ((a()))
    ((a foo))
    ((a()))
    ((a()))
    ((a()))
    ((a foo))
    ((a()))
    ((a())) |}]
;;

let%expect_test _ =
  run_single "(.* %a=[foo*] .*)" "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((a()))
    ((a foo))
    ((a(foo foo)))
    ((a()))
    ((a foo))
    ((a()))
    ((a()))
    ((a()))
    ((a foo))
    ((a()))
    ((a())) |}]
;;

let%expect_test _ =
  run_single ".. %/[abc]*/" "(bar baz foo cabbage cab)";
  [%expect {|
    :bar
    baz
    foo
    cabbage
    cab |}]
;;

let%expect_test _ =
  run_single ".. %/[abc]+/" "(bar baz foo cabbage cab)";
  [%expect {|
    :bar
    baz
    cabbage
    cab |}]
;;

let%expect_test _ =
  run_single ".. %/^[abc]+/" "(bar baz foo cabbage cab)";
  [%expect {|
    :bar
    baz
    cabbage
    cab |}]
;;

let%expect_test _ =
  run_single ".. %/^[abc]+$/" "(bar baz foo cabbage cab)";
  [%expect {|
    :cab |}]
;;

let%expect_test _ =
  run_single ".. {a .. %/c/}" "(a (foo cab a) acc (car) (a car))";
  [%expect {|
    :cab
    acc
    car
    car
    cab
    car |}]
;;

let%expect_test _ =
  run "%[/a/ & /b/ | /c/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect {|
    :
    :
    :
    :c
    :ab
    :ac
    :bc
    :abc abc |}]
;;

let%expect_test _ =
  run "%[/c/ | /a/ & /b/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect {|
    :
    :
    :
    :c
    :ab
    :ac
    :bc
    :abc abc |}]
;;

let%expect_test _ =
  run "%[/a/ & [/b/ | /c/]]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc abc |}]
;;

let%expect_test _ =
  run "%[/a/ & ![/b/ | /c/]]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc |}]
;;

let%expect_test _ =
  run "%[[![/b/ | /c/]] & /a/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc |}]
;;

let%expect_test _ =
  run
    "%[![{%[b]} | {%[c]}] & {a}]"
    [ "()"; "(a)"; "(b)"; "(c)"; "(a b)"; "(a c)"; "(b c)"; "(a b c)" ];
  [%expect
    {|
    :
    :
    :
    :
    :((a b)b())
    :((a c)()c)
    :
    :((a b c)b()) |}]
;;

let%expect_test _ =
  run_single
    "{[ (a %0 %1) | (b %0 .) ]} "
    "( (a b c) (b c a) (d b b) (b d e) (a f e) (b g g) )";
  [%expect {|
    :(b c)
    (c())
    (d())
    (f e)
    (g()) |}]
;;

let%expect_test _ =
  run_single ".. (foo (%.?))" "( (foo (1)) (foo ()) (foo (2)) (foo (())) (foo ()) )";
  [%expect {|
    :1
    ()
    2
    ()
    () |}]
;;

let%expect_test _ =
  run_single
    ~wrap_singletons:true
    ".. (foo (%.?))"
    "( (foo (1)) (foo ()) (foo (2)) (foo (())) (foo ()) )";
  [%expect {|
    :(1)
    ()
    (2)
    (())
    () |}]
;;

let%expect_test _ =
  run_single "([.*] %.)" "(a b)";
  [%expect {|
    :b |}]
;;

let%expect_test _ =
  run_single "(![.*] %.)" "(a b)";
  [%expect {|
    : |}]
;;

let%expect_test _ =
  run_single ".. %0=[a . c]" "(a b a a b c c a d c)";
  [%expect {|
    :((a b c))
    ((a d c)) |}]
;;

let%expect_test _ =
  run
    "{(foo %foo)(bar %bar)?}"
    [ "((foo 1) (bar 2))"
    ; "((bar 3) (foo 4))"
    ; "((foo 5))"
    ; "((foo 6) (baz 7))"
    ; "((foo 8) (bar 9) (baz 10))"
    ];
  [%expect
    {|
    :((foo 1)(bar())) ((foo 1)(bar())) ((foo 1)(bar 2))
    :((foo 4)(bar())) ((foo 4)(bar 3)) ((foo 4)(bar()))
    :((foo 5)(bar()))
    :((foo 6)(bar())) ((foo 6)(bar()))
    :((foo 8)(bar())) ((foo 8)(bar())) ((foo 8)(bar 9)) ((foo 8)(bar())) |}]
;;

let%expect_test _ =
  replace_single
    ".. %0=(%foo %bar)"
    ~replace:"%0"
    ~with_:"(%bar %foo)"
    "((a 1)(b 2)(c 3))";
  [%expect {|
    :((1 a)(2 b)(3 c)) |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    replace_single
      ".. %0=(%foo %bar)"
      ~replace:"%0"
      ~with_:"(%baz %foo)"
      "((a 1)(b 2)(c 3))");
  [%expect
    {|
    :(raised (
      Failure
      "Output or replacement expression uses capture not present in pattern: baz")) |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    replace_single
      ".. %0=(%foo %bar)"
      ~replace:"0"
      ~with_:"(%bar %foo)"
      "((a 1)(b 2)(c 3))");
  [%expect
    {|
    :(raised (Failure "Replacement target '0' does not start with '%'")) |}]
;;

let%expect_test _ =
  replace_single ".. %0=[a]" ~replace:"%0" ~with_:"()" "(a b a a b c c a d c)";
  [%expect {|
    :(()b()()b c c()d c) |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    replace_single ".. %0=[a . c]" ~replace:"%0" ~with_:"()" "(a b a a b c c a d c)");
  [%expect
    {|
    :(raised (
      Failure
      "Replacement target %0 captured more than one sexp, currently\n          replace only supports patterns where the replacement target captures a single sexp")) |}]
;;

let%expect_test _ =
  replace_single
    "(foo %a .. (bar %b .. %baz=(%c %d)))"
    ~replace:"%baz"
    ~with_:"(%d %c %a %b)"
    "(foo 100 (bar 25 ((a 1)(b 2)(c 3))) ((d 4)(e 5)(f 6)) (bar 30 (((g 7)(h 8)(i 9)) \
     ((j 10)(k 11)(l 12)))))";
  [%expect
    {|
    :(foo 100(bar 25((1 a 100 25)(2 b 100 25)(3 c 100 25)))((d 4)(e 5)(f 6))(bar 30(((j 10)(k 11)(l 12))((g 7)(h 8)(i 9))100 30))) |}]
;;

let%expect_test _ =
  replace_single
    "(foo %a .. (bar %b .. %baz=(%c %d)))"
    ~replace:"%baz"
    ~with_:"(%d %c %a %b)"
    "(foo 100 (bar 25 ((a 1)(b 2)(c 3))) ((d 4)(e 5)(f 6)) (bar 30 (aa ((g 7)(h 8)(i \
     9)) ((j 10)(k 11)(l 12)))))";
  [%expect
    {|
    :(foo 100(bar 25((1 a 100 25)(2 b 100 25)(3 c 100 25)))((d 4)(e 5)(f 6))(bar 30(aa((7 g 100 30)(8 h 100 30)(9 i 100 30))((10 j 100 30)(11 k 100 30)(12 l 100 30))))) |}]
;;

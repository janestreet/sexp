open Core
open Expect_test_helpers_core
open Sexp_app_pattern

module Wrap_mode_for_run = struct
  type t =
    | Output of Output_method.some_output_method
    | Wrap_non_singletons
    | Wrap_always
    | Unwrap_always

  let to_output_method query = function
    | Output output_method -> output_method
    | Wrap_non_singletons ->
      Output_method.default_method query ~wrap_mode:Wrap_non_singletons
    | Wrap_always -> Output_method.default_method query ~wrap_mode:Wrap_always
    | Unwrap_always -> Output_method.default_method query ~wrap_mode:Unwrap_always
  ;;
end

let show_parse querystr =
  let query = Parser.parse_exn querystr in
  printf "%s : %s\n" querystr (Sexp.to_string (Query.sexp_of_t query))
;;

let show_parse_fail querystr =
  printf "%s : " querystr;
  show_raise ~hide_positions:true (fun () -> Parser.parse_exn querystr)
;;

let run
  ?(more_newlines = false)
  ?(wrap_mode = Wrap_mode_for_run.Wrap_non_singletons)
  querystr
  sexpstrs
  =
  let query = Parser.parse_exn querystr in
  let output_method = Wrap_mode_for_run.to_output_method query wrap_mode in
  let (T output_method) = output_method in
  let delimiter = if more_newlines then "\n" else " " in
  List.iter sexpstrs ~f:(fun sexpstr ->
    let sexp = Sexp.of_string sexpstr in
    printf ":";
    Engine.iter_matches ~query ~output_method sexp ~f:(fun s ->
      match output_method with
      | Map -> assert false
      | Formats _ -> assert false
      | List _ -> printf !"%{Sexp}%s" s delimiter
      | Record _ -> printf !"%{Sexp}%s" s delimiter
      | Single_capture Unwrap_always ->
        List.iter s ~f:(fun s -> printf !"%{Sexp}%s" s delimiter)
      | Single_capture Wrap_non_singletons -> printf !"%{Sexp}%s" s delimiter
      | Single_capture Wrap_always -> printf !"%{Sexp}%s" s delimiter);
    printf "\n")
;;

let run_format ~format ?(wrap_mode = `Wrap_non_singletons) querystr sexpstrs =
  let wrap_mode =
    match wrap_mode with
    | `Wrap_non_singletons ->
      Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_non_singletons
    | `Wrap_always -> Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_always
    | `Unwrap_always -> Sexp_app_pattern.Output_method.Wrap_mode.T Unwrap_always
  in
  let (Sexp_app_pattern.Output_method.Wrap_mode.T wrap_mode) = wrap_mode in
  let output_method =
    Output_method.Formats (wrap_mode, Output_method.Format.ts_of_string format)
  in
  let query = Parser.parse_exn querystr in
  List.iter sexpstrs ~f:(fun sexpstr ->
    let sexp = Sexp.of_string sexpstr in
    printf ":";
    Engine.iter_matches ~query ~output_method sexp ~f:(fun s ->
      List.iter s ~f:(fun s -> printf !"%{Sexp} " s));
    printf "\n")
;;

let run_single ?wrap_mode querystr sexpstr =
  run ~more_newlines:true ?wrap_mode querystr [ sexpstr ]
;;

let replace_single ?(wrap_mode = `Wrap_non_singletons) querystr ~replace ~with_ sexpstr =
  let wrap_mode =
    match wrap_mode with
    | `Wrap_non_singletons ->
      Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_non_singletons
    | `Wrap_always -> Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_always
    | `Unwrap_always -> Sexp_app_pattern.Output_method.Wrap_mode.T Unwrap_always
  in
  let (Sexp_app_pattern.Output_method.Wrap_mode.T wrap_mode) = wrap_mode in
  let with_ = Output_method.Format.ts_of_string with_ in
  let query = Parser.parse_exn querystr in
  let sexp = Sexp.of_string sexpstr in
  printf ":";
  let result = Engine.replace ~query ~replace ~with_ sexp ~wrap_mode in
  List.iter result ~f:(fun result -> printf !"%{Sexp}\n" result)
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
  show_parse "a??";
  show_parse "(a b?? c)";
  show_parse_fail "a? ?";
  show_parse_fail "a ??";
  show_parse "a?*";
  show_parse "a*?";
  show_parse "a*+";
  show_parse_fail "a* +";
  show_parse_fail "a *+";
  show_parse "a?+";
  show_parse "a+*";
  show_parse "a+?";
  show_parse "a++";
  show_parse "a+++";
  show_parse "a++++";
  show_parse "a*++";
  show_parse "a*+++";
  show_parse "a*+*++";
  show_parse "a*++*+";
  show_parse "a**";
  show_parse "%a**";
  show_parse_fail "(ab[)]";
  show_parse "{}";
  show_parse "{a}";
  show_parse "{a b}";
  show_parse "{ a b }";
  show_parse "{[a b]}";
  show_parse "{a [b c] d}";
  show_parse "{a ()}";
  show_parse "{a b*}";
  show_parse "{ab b. . (c)}";
  show_parse "{ab {} b. {{c}}}";
  show_parse_fail "{a b *}";
  show_parse_fail "{*}";
  show_parse "{a b?}";
  show_parse_fail "{a b??}";
  show_parse "{a b+}";
  show_parse "{a b++}";
  show_parse "{a b*}";
  show_parse "{a b**}";
  show_parse "{a b? c}";
  show_parse "{a [b?] c}";
  show_parse "{a [b?]? c}";
  show_parse "{a [b??] c}";
  show_parse_fail "{a b?? c}";
  show_parse_fail "{a b? ? c}";
  show_parse "{a b*? c}";
  show_parse_fail "{a b?* c}";
  show_parse "{a [b?]* c}";
  show_parse "{a !b c}";
  show_parse "{a b? !c !d?}";
  show_parse "{!a ![!b] [!c]}";
  show_parse_fail "{!a !!b [!c]}";
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
  show_parse "%0=()";
  show_parse "%12=()";
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
  show_parse "%![a]";
  show_parse "%1=![a]";
  show_parse "%abc=![a]";
  show_parse "!a";
  show_parse "{!(a)}";
  show_parse "{!{a}}";
  show_parse "{![a]}";
  show_parse_fail "{%![a]}";
  show_parse_fail "{%1=![a]}";
  show_parse_fail "{%abc=![a]}";
  show_parse "{!a}";
  show_parse "{%[a]}";
  show_parse "{%1=[a]}";
  show_parse "{%abc=[a]}";
  show_parse "{a}";
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
    a?? : (Maybe(Maybe(Atom a)))
    (a b?? c) : (List(Sequence((Atom a)(Maybe(Maybe(Atom b)))(Atom c))))
    a? ? : (raised (Failure "Parsing match query failed at line 1 char 3 in query a? ?"))
    a ?? : (raised (Failure "Parsing match query failed at line 1 char 2 in query a ??"))
    a?* : (Star(Maybe(Atom a)))
    a*? : (Maybe(Star(Atom a)))
    a*+ : (Star_greedy(Atom a))
    a* + : (raised (Failure "Parsing match query failed at line 1 char 3 in query a* +"))
    a *+ : (raised (Failure "Parsing match query failed at line 1 char 2 in query a *+"))
    a?+ : (Maybe_greedy(Atom a))
    a+* : (Star(Plus(Atom a)))
    a+? : (Maybe(Plus(Atom a)))
    a++ : (Plus_greedy(Atom a))
    a+++ : (Plus(Plus_greedy(Atom a)))
    a++++ : (Plus_greedy(Plus_greedy(Atom a)))
    a*++ : (Plus(Star_greedy(Atom a)))
    a*+++ : (Plus_greedy(Star_greedy(Atom a)))
    a*+*++ : (Plus(Star_greedy(Star_greedy(Atom a))))
    a*++*+ : (Star_greedy(Plus(Star_greedy(Atom a))))
    a** : (Star(Star(Atom a)))
    %a** : (Star(Star(Capture_to_name a Any)))
    (ab[)] : (raised (
      Failure "Parsing match query failed at line 1 char 4 in query (ab[)]"))
    {} : (Set())
    {a} : (Set(((Atom a)((optional false)(first_only false)))))
    {a b} : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional false)(first_only false)))))
    { a b } : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional false)(first_only false)))))
    {[a b]} : (Set(((Sequence((Atom a)(Atom b)))((optional false)(first_only false)))))
    {a [b c] d} : (Set(((Atom a)((optional false)(first_only false)))((Sequence((Atom b)(Atom c)))((optional false)(first_only false)))((Atom d)((optional false)(first_only false)))))
    {a ()} : (Set(((Atom a)((optional false)(first_only false)))((List(Sequence()))((optional false)(first_only false)))))
    {a b*} : (Set(((Atom a)((optional false)(first_only false)))((Star(Atom b))((optional false)(first_only false)))))
    {ab b. . (c)} : (Set(((Atom ab)((optional false)(first_only false)))((Atom b.)((optional false)(first_only false)))(Any((optional false)(first_only false)))((List(Atom c))((optional false)(first_only false)))))
    {ab {} b. {{c}}} : (Set(((Atom ab)((optional false)(first_only false)))((Set())((optional false)(first_only false)))((Atom b.)((optional false)(first_only false)))((Set(((Set(((Atom c)((optional false)(first_only false)))))((optional false)(first_only false)))))((optional false)(first_only false)))))
    {a b *} : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query {a b *}"))
    {*} : (raised (Failure "Parsing match query failed at line 1 char 1 in query {*}"))
    {a b?} : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional true)(first_only false)))))
    {a b??} : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query {a b??}"))
    {a b+} : (Set(((Atom a)((optional false)(first_only false)))((Plus(Atom b))((optional false)(first_only false)))))
    {a b++} : (Set(((Atom a)((optional false)(first_only false)))((Plus_greedy(Atom b))((optional false)(first_only false)))))
    {a b*} : (Set(((Atom a)((optional false)(first_only false)))((Star(Atom b))((optional false)(first_only false)))))
    {a b**} : (Set(((Atom a)((optional false)(first_only false)))((Star(Star(Atom b)))((optional false)(first_only false)))))
    {a b? c} : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional true)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a [b?] c} : (Set(((Atom a)((optional false)(first_only false)))((Maybe(Atom b))((optional false)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a [b?]? c} : (Set(((Atom a)((optional false)(first_only false)))((Maybe(Atom b))((optional true)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a [b??] c} : (Set(((Atom a)((optional false)(first_only false)))((Maybe(Maybe(Atom b)))((optional false)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a b?? c} : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query {a b?? c}"))
    {a b? ? c} : (raised (
      Failure "Parsing match query failed at line 1 char 6 in query {a b? ? c}"))
    {a b*? c} : (Set(((Atom a)((optional false)(first_only false)))((Star(Atom b))((optional true)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a b?* c} : (raised (
      Failure "Parsing match query failed at line 1 char 5 in query {a b?* c}"))
    {a [b?]* c} : (Set(((Atom a)((optional false)(first_only false)))((Star(Maybe(Atom b)))((optional false)(first_only false)))((Atom c)((optional false)(first_only false)))))
    {a !b c} : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional false)(first_only true)))((Atom c)((optional false)(first_only false)))))
    {a b? !c !d?} : (Set(((Atom a)((optional false)(first_only false)))((Atom b)((optional true)(first_only false)))((Atom c)((optional false)(first_only true)))((Atom d)((optional true)(first_only true)))))
    {!a ![!b] [!c]} : (Set(((Atom a)((optional false)(first_only true)))((First_match_only(Atom b))((optional false)(first_only true)))((First_match_only(Atom c))((optional false)(first_only false)))))
    {!a !!b [!c]} : (raised (
      Failure
      "Parsing match query failed at line 1 char 5 in query {!a !!b [!c]}"))
    %1 : (Capture_to_number 1 Any)
    %012 : (Capture_to_number 12 Any)
    %-2 : (Capture_to_name -2 Any)
    %abc : (Capture_to_name abc Any)
    %a bc : (Sequence((Capture_to_name a Any)(Atom bc)))
    %a%bc : (Sequence((Capture_to_name a Any)(Capture_to_name bc Any)))
    % : (raised (Failure "Parsing match query failed at line 1 char 1 in query %"))
    %% : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%"))
    %%a : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%a"))
    % a : (raised (Failure "Parsing match query failed at line 1 char 1 in query % a"))
    %. : (Capture_unlabeled Any)
    %.a : (Capture_to_name .a Any)
    % . : (raised (Failure "Parsing match query failed at line 1 char 1 in query % ."))
    %.. : (raised (Failure "Parsing match query failed at line 1 char 1 in query %.."))
    a%1 : (Sequence((Atom a)(Capture_to_number 1 Any)))
    %1* : (Star(Capture_to_number 1 Any))
    %a* : (Star(Capture_to_name a Any))
    %[a]* : (Star(Capture_unlabeled(Atom a)))
    %a+ : (Plus(Capture_to_name a Any))
    %[a]+ : (Plus(Capture_unlabeled(Atom a)))
    %a? : (Maybe(Capture_to_name a Any))
    %() : (Capture_unlabeled(List(Sequence())))
    %%() : (raised (Failure "Parsing match query failed at line 1 char 1 in query %%()"))
    %(%a) : (Capture_unlabeled(List(Capture_to_name a Any)))
    %[%a] : (Capture_unlabeled(Capture_to_name a Any))
    %{%.} : (Capture_unlabeled(Set(((Capture_unlabeled Any)((optional false)(first_only false))))))
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
    { a .. b } : (Set(((Atom a)((optional false)(first_only false)))((Subsearch(Atom b))((optional false)(first_only false)))))
    {a .. b .. c} : (Set(((Atom a)((optional false)(first_only false)))((Subsearch(Sequence((Atom b)(Subsearch(Atom c)))))((optional false)(first_only false)))))
    a .. %b : (Sequence((Atom a)(Subsearch(Capture_to_name b Any))))
    a .. b* : (Sequence((Atom a)(Subsearch(Star(Atom b)))))
    %a? .. b : (Sequence((Maybe(Capture_to_name a Any))(Subsearch(Atom b))))
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
    {[a | b]} : (Set(((Or_all((Atom a)(Atom b)))((optional false)(first_only false)))))
    %a=b&c : (And((Capture_to_name a(Atom b))(Atom c)))
    %a=() : (Capture_to_name a(List(Sequence())))
    %a = b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query %a = b"))
    %()=b : (raised (Failure "Parsing match query failed at line 1 char 3 in query %()=b"))
    %a=b* : (Star(Capture_to_name a(Atom b)))
    %abc=()* : (Star(Capture_to_name abc(List(Sequence()))))
    %0=() : (Capture_to_number 0(List(Sequence())))
    %12=() : (Capture_to_number 12(List(Sequence())))
    %1=()* : (Star(Capture_to_number 1(List(Sequence()))))
    %1=a..b : (Capture_to_number 1(Atom a..b))
    %1=a .. b : (Sequence((Capture_to_number 1(Atom a))(Subsearch(Atom b))))
    %a=.. b : (raised (
      Failure "Parsing match query failed at line 1 char 3 in query %a=.. b"))
    %a=[.. b] : (Capture_to_name a(Subsearch(Atom b)))
    %a=* : (raised (Failure "Parsing match query failed at line 1 char 3 in query %a=*"))
    %"a=*" : (Capture_unlabeled(Atom a=*))
    ( %a=b c d ) : (List(Sequence((Capture_to_name a(Atom b))(Atom c)(Atom d))))
    { %a=b c d } : (Set(((Capture_to_name a(Atom b))((optional false)(first_only false)))((Atom c)((optional false)(first_only false)))((Atom d)((optional false)(first_only false)))))
    !(a) : (First_match_only(List(Atom a)))
    !{a} : (First_match_only(Set(((Atom a)((optional false)(first_only false))))))
    ![a] : (First_match_only(Atom a))
    %![a] : (Capture_unlabeled(First_match_only(Atom a)))
    %1=![a] : (Capture_to_number 1(First_match_only(Atom a)))
    %abc=![a] : (Capture_to_name abc(First_match_only(Atom a)))
    !a : (First_match_only(Atom a))
    {!(a)} : (Set(((List(Atom a))((optional false)(first_only true)))))
    {!{a}} : (Set(((Set(((Atom a)((optional false)(first_only false)))))((optional false)(first_only true)))))
    {![a]} : (Set(((Atom a)((optional false)(first_only true)))))
    {%![a]} : (raised (
      Failure "Parsing match query failed at line 1 char 2 in query {%![a]}"))
    {%1=![a]} : (raised (
      Failure "Parsing match query failed at line 1 char 4 in query {%1=![a]}"))
    {%abc=![a]} : (raised (
      Failure "Parsing match query failed at line 1 char 6 in query {%abc=![a]}"))
    {!a} : (Set(((Atom a)((optional false)(first_only true)))))
    {%[a]} : (Set(((Capture_unlabeled(Atom a))((optional false)(first_only false)))))
    {%1=[a]} : (Set(((Capture_to_number 1(Atom a))((optional false)(first_only false)))))
    {%abc=[a]} : (Set(((Capture_to_name abc(Atom a))((optional false)(first_only false)))))
    {a} : (Set(((Atom a)((optional false)(first_only false)))))
    !.. a : (raised (Failure "Parsing match query failed at line 1 char 1 in query !.. a"))
    a !()!() : (Sequence((Atom a)(First_match_only(List(Sequence())))(First_match_only(List(Sequence())))))
    |}]
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
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))
    |}]
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
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))
    |}]
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
    :
    |}]
;;

let%expect_test _ =
  run "(%.)" standard_test_cases;
  [%expect
    {|
    :
    :
    :()
    :abc
    :(abc)
    :
    :
    :
    :
    :
    |}]
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
    :
    |}]
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
    :((a 1)(b 2)(c 3)) ((d 3)(e 4)(f 5))
    |}]
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
    :((((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))))
    |}]
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
    :((field(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))))
    |}]
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
    :
    |}]
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
    :(((a 1)(b 2)(c 3))) (((d 3)(e 4)(f 5)))
    |}]
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
    :((foo(a 1))(bar(b 2))(baz(c 3))) ((foo(d 3))(bar(e 4))(baz(f 5)))
    |}]
;;

let%expect_test _ =
  run_format
    ~format:"(%foo ((%baz)) (barr (%bar)))"
    ".. (%foo %bar %baz)"
    standard_test_cases;
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
    :((a 1)(((c 3)))(barr((b 2)))) ((d 3)(((f 5)))(barr((e 4))))
    |}]
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
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))
    |}]
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
    :((a 1)(b 2)(c 3)) ((d 3)(e 4)(f 5))
    |}]
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
    :((d 3)(e 4)(f 5))
    |}]
;;

let%expect_test _ =
  run ".. b %." standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :c
    :
    :2
    :2
    :2
    |}]
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
    :((b 2)b) ((b 2)2)
    |}]
;;

let%expect_test _ =
  run ".. (.? %. 3)" standard_test_cases;
  [%expect
    {|
    :
    :
    :
    :
    :
    :
    :
    :c
    :3
    :c d
    |}]
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
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5))) (a 1) (b 2) (c 3) (d 3) (e 4) (f 5)
    |}]
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
    :(((a 1)(b 2)(c 3))((d 3)(e 4)(f 5)))
    |}]
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
    :(((d 3)(e 4)(f 5))()) ((b 2)()) ((b 2)(c 3)) (1()) ((c 3)()) (2()) (3()) ((e 4)()) ((e 4)(f 5)) (3()) ((f 5)()) (4()) (5())
    |}]
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
    :(((d 3)(e 4)(f 5))()) ((b 2)(c 3)) (1()) (2()) (3()) ((e 4)(f 5)) (3()) (4()) (5())
    |}]
;;

let%expect_test _ =
  run ".. (a %.) & .. (c %.) " standard_test_cases;
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
    :(1 3)
    |}]
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
    :((a 1)(b 2))
    |}]
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
    :((a 1)(b 2))
    |}]
;;

let%expect_test _ =
  run "[%[[([a])]]]" [ "a"; "(a)"; "()"; "(a a)" ];
  [%expect
    {|
    :
    :(a)
    :
    :
    |}]
;;

let%expect_test _ =
  run_single "(%a %1)" "(c d)";
  [%expect {| :((a c)(1 d)) |}]
;;

let%expect_test _ =
  run_single "(%1 %0)" "(c d)";
  [%expect {| :(d c) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "." "(c d)");
  [%expect {| (raised (Failure "No captures % were specified in pattern")) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%2 %1)" "(c d)");
  [%expect
    {|
    :(raised (
      Failure
      "Match pattern uses captures up to %2 but is missing %0 (reminder: numbered captures should be zero-indexed)"))
    |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%2 %0)" "(c d)");
  [%expect
    {| :(raised (Failure "Match pattern uses captures up to %2 but is missing %1")) |}]
;;

let%expect_test _ =
  show_raise (fun () -> run_single "(%0 %.)" "(c d)");
  [%expect
    {|
    :(raised (
      Failure
      "Cannot mix unlabeled captures with named or numbered captures in the same pattern"))
    |}]
;;

let%expect_test _ =
  run_single ".. (foo (.* %. .*))" "((foo (A B C)) (bar (D E F)) (foo (foo (G H I))))";
  [%expect
    {|
    :A
    B
    C
    foo
    (G H I)
    G
    H
    I
    |}]
;;

let%expect_test _ =
  run_single ".. (foo (%[.*]))" "((foo (A B C)) (bar (D E F)) (foo (foo (G H I))))";
  [%expect
    {|
    :(A B C)
    (foo(G H I))
    (G H I)
    |}]
;;

let%expect_test _ =
  run_single "(.* %[foo]+ .*)" "(foo foo bar baz foo bar)";
  (* yes, we get FOUR of them. The extra foo comes from when %[foo]+ matches "foo foo",
     and since the capture is inside the +, it successively captures the first "foo" and
     then second "foo" which overwrites the first. *)
  [%expect
    {|
    :foo
    foo
    foo
    foo
    |}]
;;

[@@@ocamlformat "disable"]

let%expect_test _ =
  run_single
    ~wrap_mode:Unwrap_always
    "(.* %[foo+] .*)"
    "(foo foo bar baz foo bar)";
  [%expect {|
    :foo
    foo
    foo
    foo
    foo
    |}]

let%expect_test _ =
  run_single
    ~wrap_mode:Wrap_non_singletons
    "(.* %[foo+] .*)"
    "(foo foo bar baz foo bar)";
  [%expect {|
    :foo
    (foo foo)
    foo
    foo
    |}]

let%expect_test _ =
  run_single
    ~wrap_mode:Wrap_always
    "(.* %[foo+] .*)"
    "(foo foo bar baz foo bar)";
  [%expect {|
    :(foo)
    (foo foo)
    (foo)
    (foo)
    |}]

let%expect_test _ =
  run_single
    ~wrap_mode:Unwrap_always
    "(.* %[foo+] .* %.)"
    "(foo foo bar baz foo bar)";
  [%expect {|
    :(foo bar)
    (foo foo bar)
    (foo bar)
    (foo bar)
    |}]

let%expect_test _ =
  run_single
    ~wrap_mode:Wrap_non_singletons
    "(.* %[foo+] .* %.)"
    "(foo foo bar baz foo bar)";
  [%expect {|
    :(foo bar)
    ((foo foo)bar)
    (foo bar)
    (foo bar)
    |}]

let%expect_test _ =
  run_single
    ~wrap_mode:Wrap_always
    "(.* %[foo+] .* %.)"
    "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((foo)(bar))
    ((foo foo)(bar))
    ((foo)(bar))
    ((foo)(bar))
    |}]
[@@@ocamlformat "enable"]

let%expect_test _ =
  run_single "(.* %a=foo+ .*)" "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((a foo))
    ((a foo))
    ((a foo))
    ((a foo))
    |}]
;;

let%expect_test _ =
  run_single "(.* %a=[foo+] .*)" "(foo foo bar baz foo bar)";
  [%expect
    {|
    :((a foo))
    ((a(foo foo)))
    ((a foo))
    ((a foo))
    |}]
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
    ((a()))
    |}]
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
    ((a()))
    |}]
;;

let%expect_test _ =
  run_single ".. %/[abc]*/" "(bar baz foo cabbage cab)";
  [%expect
    {|
    :bar
    baz
    foo
    cabbage
    cab
    |}]
;;

let%expect_test _ =
  run_single ".. %/[abc]+/" "(bar baz foo cabbage cab)";
  [%expect
    {|
    :bar
    baz
    cabbage
    cab
    |}]
;;

let%expect_test _ =
  run_single ".. %/^[abc]+/" "(bar baz foo cabbage cab)";
  [%expect
    {|
    :bar
    baz
    cabbage
    cab
    |}]
;;

let%expect_test _ =
  run_single ".. %/^[abc]+$/" "(bar baz foo cabbage cab)";
  [%expect {| :cab |}]
;;

let%expect_test _ =
  run_single ".. {a .. %/c/}" "(a (foo cab a) acc (car) (a car))";
  [%expect
    {|
    :cab
    acc
    car
    car
    cab
    car
    |}]
;;

let%expect_test _ =
  run "%[/a/ & /b/ | /c/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect
    {|
    :
    :
    :
    :c
    :ab
    :ac
    :bc
    :abc abc
    |}]
;;

let%expect_test _ =
  run "%[/c/ | /a/ & /b/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect
    {|
    :
    :
    :
    :c
    :ab
    :ac
    :bc
    :abc abc
    |}]
;;

let%expect_test _ =
  run "%[/a/ & [/b/ | /c/]]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect
    {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc abc
    |}]
;;

let%expect_test _ =
  run "%[/a/ & ![/b/ | /c/]]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect
    {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc
    |}]
;;

let%expect_test _ =
  run "%[[![/b/ | /c/]] & /a/]" [ "d"; "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ];
  [%expect
    {|
    :
    :
    :
    :
    :ab
    :ac
    :
    :abc
    |}]
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
    :((a b c)b())
    |}]
;;

let%expect_test _ =
  run_single
    "{[ (a %0 %1) | (b %0 .) ]} "
    "( (a b c) (b c a) (d b b) (b d e) (a f e) (b g g) )";
  [%expect
    {|
    :(b c)
    (c())
    (d())
    (f e)
    (g())
    |}]
;;

let%expect_test _ =
  run_single ".. (foo (%.?))" "( (foo (1)) (foo ()) (foo (2)) (foo (())) (foo ()) )";
  [%expect
    {|
    :1
    ()
    2
    ()
    ()
    |}]
;;

let%expect_test _ =
  run_single
    ~wrap_mode:Wrap_always
    ".. (foo (%.?))"
    "( (foo (1)) (foo ()) (foo (2)) (foo (())) (foo ()) )";
  [%expect
    {|
    :(1)
    ()
    (2)
    (())
    ()
    |}]
;;

let%expect_test _ =
  run_single "([[%.]*]+)" "(a b)";
  [%expect
    {|
    :b
    b
    b
    b
    |}]
;;

let%expect_test _ =
  (* Here the matches correspond to:
     1. [a] [b] []
     2. [a] [b]
     3. [a b] []
     4. [a b]
  *)
  run_single "([[%.]*]++)" "(a b)";
  [%expect
    {|
    :b
    b
    b
    b
    |}]
;;

let%expect_test _ =
  run_single "([.*] %.)" "(a b)";
  [%expect {| :b |}]
;;

let%expect_test _ =
  run_single "(![.*] %.)" "(a b)";
  [%expect {| : |}]
;;

let%expect_test _ =
  run_single ".. %0=[a . c]" "(a b a a b c c a d c)";
  [%expect
    {|
    :((a b c))
    ((a d c))
    |}]
;;

let%expect_test _ =
  run
    "{(foo %foo)(bar %bar)?}"
    [ "((foo 1) (bar 2))"
    ; "((bar 3) (foo 4))"
    ; "((foo 5))"
    ; "((foo 6) (baz 7))"
    ; "((foo 8) (bar 9) (baz 10))"
    ; "()"
    ];
  [%expect
    {|
    :((foo 1)(bar 2))
    :((foo 4)(bar 3))
    :((foo 5)(bar()))
    :((foo 6)(bar()))
    :((foo 8)(bar 9))
    :
    |}]
;;

let%expect_test _ =
  run
    "{(bar %bar)?}"
    [ "((foo 1) (bar 2))"
    ; "((bar 3) (foo 4))"
    ; "((foo 5))"
    ; "((foo 6) (baz 7))"
    ; "((foo 8) (bar 9) (baz 10))"
    ; "()"
    ];
  [%expect
    {|
    :((bar 2))
    :((bar 3))
    :((bar()))
    :((bar()))
    :((bar 9))
    :((bar()))
    |}]
;;

let%expect_test _ =
  run
    "{(baz %baz)?(foo %foo)(bar %bar)? (aaa %aaa)?(%a %b)?}"
    [ "((foo 1) (bar 2))"
    ; "((bar 3) (foo 4))"
    ; "((foo 5))"
    ; "((foo 6) (baz 7))"
    ; "((foo 8) (bar 9) (baz 10))"
    ];
  [%expect
    {|
    :((baz())(foo 1)(bar 2)(aaa())(a foo)(b 1)) ((baz())(foo 1)(bar 2)(aaa())(a bar)(b 2))
    :((baz())(foo 4)(bar 3)(aaa())(a bar)(b 3)) ((baz())(foo 4)(bar 3)(aaa())(a foo)(b 4))
    :((baz())(foo 5)(bar())(aaa())(a foo)(b 5))
    :((baz 7)(foo 6)(bar())(aaa())(a foo)(b 6)) ((baz 7)(foo 6)(bar())(aaa())(a baz)(b 7))
    :((baz 10)(foo 8)(bar 9)(aaa())(a foo)(b 8)) ((baz 10)(foo 8)(bar 9)(aaa())(a bar)(b 9)) ((baz 10)(foo 8)(bar 9)(aaa())(a baz)(b 10))
    |}]
;;

let%expect_test _ =
  run
    "{(foo %foo)(bar .. (baz %baz))?}"
    [ "((foo 1) (bar ()))"
    ; "((foo 1) (bar (baz 2)))"
    ; "((foo 1) (bar ((baz 3) (baz 4))))"
    ; "((foo 1) (bar ((baz 3) (baz 4))) (bar ((baz 5) (baz 6))))"
    ];
  [%expect
    {|
    :((foo 1)(baz()))
    :((foo 1)(baz 2))
    :((foo 1)(baz 3)) ((foo 1)(baz 4))
    :((foo 1)(baz 3)) ((foo 1)(baz 4)) ((foo 1)(baz 5)) ((foo 1)(baz 6))
    |}]
;;

let%expect_test _ =
  run "(a %x=[b?] %y=[[a|b]*])" [ "(a b b)"; "(a b a)"; "(a a)"; "(a b)" ];
  [%expect
    {|
    :((x())(y(b b))) ((x b)(y b))
    :((x())(y(b a))) ((x b)(y a))
    :((x())(y a))
    :((x())(y b)) ((x b)(y()))
    |}]
;;

let%expect_test _ =
  run "(a %x=[b?+] %y=[[a|b]*])" [ "(a b b)"; "(a b a)"; "(a a)"; "(a b)" ];
  [%expect
    {|
    :((x b)(y b)) ((x())(y(b b)))
    :((x b)(y a)) ((x())(y(b a)))
    :((x())(y a))
    :((x b)(y())) ((x())(y b))
    |}]
;;

let%expect_test _ =
  let sexps = [ "()"; "((x y) (z))" ] in
  run "{(.* %. .*)?}" sexps;
  [%expect
    {|
    :()
    :x y z
    |}];
  run "{[!(.* %. .*)]?}" sexps;
  [%expect
    {|
    :()
    :x z
    |}];
  run "{!(.* %. .*)?}" sexps;
  [%expect
    {|
    :()
    :x y
    |}];
  run "{(.* %. .*)}" sexps;
  [%expect
    {|
    :
    :x y z
    |}];
  run "{!(.* %. .*)}" sexps;
  [%expect
    {|
    :
    :x y
    |}];
  run "{[!(.* %. .*)]}" sexps;
  [%expect
    {|
    :
    :x z
    |}];
  run "{![!(.* %. .*)]}" sexps;
  [%expect
    {|
    :
    :x
    |}]
;;

let%expect_test _ =
  run "{[%foo=[.*]]?}" [ "((foo 1))"; "()"; "((foo 1) (bar 2))"; "a"; "(a)" ];
  [%expect
    {|
    :((foo(foo 1)))
    :((foo()))
    :((foo(foo 1))) ((foo(bar 2)))
    :
    :((foo a))
    |}]
;;

let%expect_test _ =
  run "([. a]* %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :1 2 3 4 5 6 |}]
;;

let%expect_test _ =
  run "([. a]*+ %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :6 5 4 3 2 1 |}]
;;

let%expect_test _ =
  run "([. a]* %. .*+)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :1 2 3 4 5 6 |}]
;;

let%expect_test _ =
  run "([. a]*+ %. .*+)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :6 5 4 3 2 1 |}]
;;

let%expect_test _ =
  run "([. a]+ %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :2 3 4 5 6 |}]
;;

let%expect_test _ =
  run "([. a]++ %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :6 5 4 3 2 |}]
;;

let%expect_test _ =
  run "([. a]+ %. .++)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :2 3 4 5 |}]
;;

let%expect_test _ =
  run "([. a]++ %. .++)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :5 4 3 2 |}]
;;

let%expect_test _ =
  run "([. a]? %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :1 2 |}]
;;

let%expect_test _ =
  run "([. a]?+ %. .*)" [ "(1 a 2 a 3 a 4 a 5 a 6)" ];
  [%expect {| :2 1 |}]
;;

let%expect_test _ =
  run
    "!(%[a?]%[b?]%[c?]%[/[^b]/*])"
    [ "(a b c d e)"
    ; "(b c d e)"
    ; "(a c d e)"
    ; "(b d e)"
    ; "(c d e)"
    ; "(c a d e)"
    ; "(d e)"
    ];
  [%expect
    {|
    :(a b()(c d e))
    :(()b()(c d e))
    :(()()()(a c d e))
    :(()b()(d e))
    :(()()()(c d e))
    :(()()()(c a d e))
    :(()()()(d e))
    |}]
;;

let%expect_test _ =
  run
    "!(%[a?+]%[b?+]%[c?+]%[/[^b]/*])"
    [ "(a b c d e)"
    ; "(b c d e)"
    ; "(a c d e)"
    ; "(b d e)"
    ; "(c d e)"
    ; "(c a d e)"
    ; "(d e)"
    ];
  [%expect
    {|
    :(a b c(d e))
    :(()b c(d e))
    :(a()c(d e))
    :(()b()(d e))
    :(()()c(d e))
    :(()()c(a d e))
    :(()()()(d e))
    |}]
;;

let%expect_test _ =
  run
    "!(%[/[^c]/*] %[/[^a]/*])"
    [ "(a a a a a b b b b b c c c c c)"
    ; "(b b b b b c c c c c)"
    ; "(a a a a a b b b b b)"
    ];
  [%expect
    {|
    :((a a a a a)(b b b b b c c c c c))
    :(()(b b b b b c c c c c))
    :((a a a a a)(b b b b b))
    |}]
;;

let%expect_test _ =
  run
    "!(%[/[^c]/+] %[/[^a]/+])"
    [ "(a a a a a b b b b b c c c c c)"
    ; "(b b b b b c c c c c)"
    ; "(a a a a a b b b b b)"
    ];
  [%expect
    {|
    :((a a a a a)(b b b b b c c c c c))
    :(b(b b b b c c c c c))
    :((a a a a a)(b b b b b))
    |}]
;;

let%expect_test _ =
  run
    "!(%[/[^c]/*+] %[/[^a]/*+])"
    [ "(a a a a a b b b b b c c c c c)"
    ; "(b b b b b c c c c c)"
    ; "(a a a a a b b b b b)"
    ];
  [%expect
    {|
    :((a a a a a b b b b b)(c c c c c))
    :((b b b b b)(c c c c c))
    :((a a a a a b b b b b)())
    |}]
;;

let%expect_test _ =
  run
    "!(%[/[^c]/++] %[/[^a]/++])"
    [ "(a a a a a b b b b b c c c c c)"
    ; "(b b b b b c c c c c)"
    ; "(a a a a a b b b b b)"
    ];
  [%expect
    {|
    :((a a a a a b b b b b)(c c c c c))
    :((b b b b b)(c c c c c))
    :((a a a a a b b b b)b)
    |}]
;;(* Simple wrapping test for direct output, list output, record output *)



[@@@ocamlformat "disable"]

module%test [@name "Simple wrapping test for direct output"] _ = struct

  (* Anywhere that an "a" occurs in the sexp, greedily capture the entire sequence
     of sexps after it *)
  let query = ".. a %![.*+]"

  let%expect_test _ =
    run_single
      ~wrap_mode:Unwrap_always
      query
      "((a b) (a c d e) (f g a) (h a i))";
    [%expect {|
      :b
      c
      d
      e
      i
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_non_singletons
      query
      "((a b) (a c d e) (f g a) (h a i))";
    [%expect {|
      :b
      (c d e)
      ()
      i
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_always
      query
      "((a b) (a c d e) (f g a) (h a i))";
    [%expect {|
      :(b)
      (c d e)
      ()
      (i)
      |}]

end

module%test [@name "Simple wrapping test for list output"] _ = struct

  (* Anywhere that an "a" occurs in the sexp, greedily capture the entire sequence
     of sexps after it, AND anywhere that a "z" occurs in the sexp, greedily
     capture the entire sequence of sexps after it.*)
  let query = "[.. a %0=![.*+]] & [.. z %1=![.*+]]  "

  let%expect_test _ =
    run_single
      ~wrap_mode:Unwrap_always
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :(b)
      (b y)
      (b x w)
      (c d)
      (c d y)
      (c d x w)
      ()
      (y)
      (x w)
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_non_singletons
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :(b())
      (b y)
      (b(x w))
      ((c d)())
      ((c d)y)
      ((c d)(x w))
      (()())
      (()y)
      (()(x w))
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_always
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :((b)())
      ((b)(y))
      ((b)(x w))
      ((c d)())
      ((c d)(y))
      ((c d)(x w))
      (()())
      (()(y))
      (()(x w))
      |}]

end

module%test [@name "Simple wrapping test for record output"] _ = struct

  (* Anywhere that an "a" occurs in the sexp, greedily capture the entire sequence
     of sexps after it, AND anywhere that a "z" occurs in the sexp, greedily
     capture the entire sequence of sexps after it.*)
  let query = "[.. a %a=![.*+]] & [.. z %z=![.*+]]  "

  let%expect_test _ =
    run_single
      ~wrap_mode:Unwrap_always
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :((a b)(z))
      ((a b)(z y))
      ((a b)(z x w))
      ((a c d)(z))
      ((a c d)(z y))
      ((a c d)(z x w))
      ((a)(z))
      ((a)(z y))
      ((a)(z x w))
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_non_singletons
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :((a b)(z()))
      ((a b)(z y))
      ((a b)(z(x w)))
      ((a(c d))(z()))
      ((a(c d))(z y))
      ((a(c d))(z(x w)))
      ((a())(z()))
      ((a())(z y))
      ((a())(z(x w)))
      |}]

  let%expect_test _ =
    run_single
      ~wrap_mode:Wrap_always
      query
      "((a b) (a c d) (a) (z) (z y) (z x w))";
    [%expect {|
      :((a(b))(z()))
      ((a(b))(z(y)))
      ((a(b))(z(x w)))
      ((a(c d))(z()))
      ((a(c d))(z(y)))
      ((a(c d))(z(x w)))
      ((a())(z()))
      ((a())(z(y)))
      ((a())(z(x w)))
      |}]

end
[@@@ocamlformat "enable"]

let%expect_test _ =
  replace_single
    ".. %0=(%foo %bar)"
    ~replace:"%0"
    ~with_:"(%bar %foo)"
    "((a 1)(b 2)(c 3))";
  [%expect {| :((1 a)(2 b)(3 c)) |}]
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
      "Output or replacement expression uses capture not present in pattern: baz"))
    |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    replace_single
      ".. %0=(%foo %bar)"
      ~replace:"0"
      ~with_:"(%bar %foo)"
      "((a 1)(b 2)(c 3))");
  [%expect {| :(raised (Failure "Replacement target '0' does not start with '%'")) |}]
;;

let%expect_test _ =
  replace_single
    "{(label %label) .. %a={ (id %id) (counts %counts) }}"
    ~replace:"%a"
    ~with_:"(%label %id %counts)"
    "((label B) ( ((id X) (counts (1 2 3))) ((id Y) (counts (4 5 6))) ) )";
  [%expect {| :((label B)((B X(1 2 3))(B Y(4 5 6)))) |}]
;;

let%expect_test _ =
  replace_single ".. %0=[a]" ~replace:"%0" ~with_:"()" "(a b a a b c c a d c)";
  [%expect {| :(()b()()b c c()d c) |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    replace_single ".. %0=[a . c]" ~replace:"%0" ~with_:"()" "(a b a a b c c a d c)");
  [%expect
    {|
    :(a b a()c())
    "did not raise"
    |}]
;;

let%expect_test _ =
  replace_single
    "(foo %a .. (bar %b .. %baz=(%c %d)))"
    ~replace:"%baz"
    ~with_:"(%d %c %a %b)"
    "(foo 100 (bar 25 ((a 1)(b 2)(c 3))) ((d 4)(e 5)(f 6)) (bar 30 (((g 7)(h 8)(i 9)) \
     ((j 10)(k 11)(l 12)))))";
  [%expect
    {| :(foo 100(bar 25((1 a 100 25)(2 b 100 25)(3 c 100 25)))((d 4)(e 5)(f 6))(bar 30(((j 10)(k 11)(l 12))((g 7)(h 8)(i 9))100 30))) |}]
;;

let%expect_test _ =
  replace_single
    "(foo %a .. (bar %b .. %baz=(%c %d)))"
    ~replace:"%baz"
    ~with_:"(%d %c %a %b)"
    "(foo 100 (bar 25 ((a 1)(b 2)(c 3))) ((d 4)(e 5)(f 6)) (bar 30 (aa ((g 7)(h 8)(i 9)) \
     ((j 10)(k 11)(l 12)))))";
  [%expect
    {| :(foo 100(bar 25((1 a 100 25)(2 b 100 25)(3 c 100 25)))((d 4)(e 5)(f 6))(bar 30(aa((7 g 100 30)(8 h 100 30)(9 i 100 30))((10 j 100 30)(11 k 100 30)(12 l 100 30))))) |}]
;;

let%expect_test "maps come out like records" =
  let query = Parser.parse_exn {|((foo %foo) %bar %baz*) | (%alt)|} in
  let test =
    let test_one sexp =
      Engine.iter_matches ~query ~output_method:Map sexp ~f:(fun s ->
        [%sexp_of: Sexp.t list String.Map.t] s |> Sexp.to_string_mach |> print_endline)
    in
    [ "((foo x) y)"; "((foo a) b c)"; "((foo 1) 2 3 4)"; "(one)"; "(a b)" ]
    |> List.map ~f:Sexp.of_string
    |> List.iter ~f:test_one
  in
  test;
  [%expect
    {|
    ((alt())(bar(y))(baz())(foo(x)))
    ((alt())(bar(b))(baz(c))(foo(a)))
    ((alt())(bar(2))(baz(4))(foo(1)))
    ((alt(one))(bar())(baz())(foo()))
    |}]
;;

let%expect_test "replace with function" =
  let query = Parser.parse_exn {|(%a=[[foo | bar]*])|} in
  Engine.replace'
    ~query
    ~f:(fun captured_sequences ->
      Map.map captured_sequences ~f:(fun captured_sequence ->
        print_s ([%sexp_of: Sexp.t list] captured_sequence);
        List.filter_map captured_sequence ~f:(fun sexp ->
          match sexp with
          | Atom "foo" -> Some (Sexp.Atom "was foo")
          | Atom "bar" -> Some (Sexp.Atom "was bar")
          | _ -> None)))
    (Sexp.of_string {|(foo bar bar foo foo)|})
  |> List.iter ~f:print_s;
  [%expect
    {|
    (foo bar bar foo foo)
    ("was foo" "was bar" "was bar" "was foo" "was foo")
    |}]
;;

let%expect_test "replace with function highly path-dependent" =
  let query = Parser.parse_exn {|.. %a=[[foo | bar][foo | bar]+]|} in
  Engine.replace'
    ~query
    ~f:(fun captured_sequences ->
      Map.map captured_sequences ~f:(fun captured_sequence ->
        print_s ([%sexp_of: Sexp.t list] captured_sequence);
        if List.is_empty captured_sequence
        then []
        else
          [ Sexp.Atom "!"
          ; Sexp.List
              (List.filter_map captured_sequence ~f:(fun sexp ->
                 match sexp with
                 | Atom "foo" -> Some (Sexp.Atom "was foo")
                 | Atom "bar" -> Some (Sexp.Atom "was bar")
                 | _ -> None))
          ]))
    (Sexp.of_string {|(foo bar bar goo foo (foo bar))|})
  |> List.iter ~f:print_s;
  [%expect
    {|
    (foo bar)
    (foo bar bar)
    (bar bar)
    (foo bar)
    (! ("was foo" "was bar") bar goo foo (! ("was foo" "was bar")))
    |}]
;;

let%expect_test "replace missing target" =
  show_raise (fun () ->
    replace_single ".. %abc=()" ~replace:"%def" ~with_:"(ghi)" "((a 1)(b 2)(c 3))");
  [%expect
    {|
    :(raised (
      Failure
      "Attempting to replace %def but it does not occur in the query pattern"))
    |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    run_single
      ~wrap_mode:
        (Output
           (T (Formats (Wrap_non_singletons, Output_method.Format.ts_of_string "()"))))
      "%."
      "abc");
  [%expect
    {|
    :(raised (
      "Query pattern contains unlabeled capture, but they are not allowed when using this output method"
      (query_pattern (Capture_unlabeled Any))
      (output_method (Formats Wrap_non_singletons (())))))
    |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    run_single ~wrap_mode:(Output (T (List Wrap_non_singletons))) "%a" "abc");
  [%expect
    {|
    :(raised (
      "Query pattern contains named capture, but they are not allowed when using this output method"
      (query_pattern (Capture_to_name a Any))
      (output_method (List Wrap_non_singletons))))
    |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    run_single ~wrap_mode:(Output (T (Record Wrap_non_singletons))) "%." "abc");
  [%expect
    {|
    :(raised (
      "Query pattern contains unlabeled capture, but they are not allowed when using this output method"
      (query_pattern (Capture_unlabeled Any))
      (output_method (Record            Wrap_non_singletons))))
    |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    run_single ~wrap_mode:(Output (T (Single_capture Wrap_non_singletons))) "%. %." "abc");
  [%expect
    {|
    :(raised (
      "Query pattern has 0 or multiple unlabeled captures, which is not allowed if using this output method"
      (query_pattern (
        Sequence (
          (Capture_unlabeled Any)
          (Capture_unlabeled Any))))
      (output_method (Single_capture Wrap_non_singletons))))
    |}]
;;

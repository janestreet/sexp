open Core

let%test_unit _ =
  let change_tests =
    [ (* const *)
      "(const 13)", "foo", Some "13"
    ; "(const 13)", "()", Some "13"
    ; "(const 13)", "(foo bar)", Some "13"
    ; (* alt *)
      "(alt)", "()", None
    ; (* delete *)
      "delete", "foo", None
    ; "(seq delete fail)", "foo", None
    ; "(children delete)", "()", Some "()"
    ; "(children delete)", "(foo bar)", Some "()"
    ; "(children (alt (rewrite foo bar) delete))", "(foo zzz)", Some "(bar)"
    ; (* record *)
      "(record)", "foo", None
    ; "(record)", "()", Some "()"
    ; "(record)", "(foo)", None
    ; "(record)", "((foo 13))", Some "((foo 13))"
    ; "(record (foo (const 13)))", "((foo foo))", Some "((foo 13))"
    ; "(record (foo (const 13)))", "((foo foo) (bar bar))", Some "((foo 13) (bar bar))"
    ; ( "(record (foo (const 13)) (_ id))"
      , "((foo foo) (bar bar))"
      , Some "((foo 13) (bar bar))" )
    ; "(record (_ delete))", "((foo foo) (bar bar))", Some "()"
    ; "(record (foo id) (_ delete))", "((foo 13))", Some "((foo 13))"
    ; "(record (foo id) (_ delete))", "((foo 13) (bar 14))", Some "((foo 13))"
    ; "(record (foo id) (_ fail))", "((foo 13))", Some "((foo 13))"
    ; "(record (foo id) (_ fail))", "((foo 13) (bar 14))", None
    ; "(record (foo            id))", "()", None
    ; "(record (foo (present ) id))", "()", None
    ; "(record (foo (absent  ) id))", "()", Some "((foo ()))"
    ; "(record (foo (optional) id))", "()", Some "((foo ()))"
    ; "(record (foo ((rename bar)) id))", "((foo 13))", Some "((bar 13))"
    ; ( "(record (foo id) (bar (const 13)) (baz (rewrite $X ($X $X))))"
      , "((foo 13) (bar 14) (baz 15) (bap 16))"
      , Some "((foo 13) (bar 13) (baz (15 15)) (bap 16))" )
    ; "(record (a1 (optional) (const foo)))", "()", Some "((a1 foo))"
    ; ( "(record (a1 (optional) (const foo)) (a2 id))"
      , "((a2 v2) (a3 v3))"
      , Some "((a2 v2) (a3 v3) (a1 foo))" )
    ]
  in
  let module Bug = struct
    type t =
      { program : string
      ; syntax : Syntax.Change.t
      ; input : string
      ; output : Sexp.t option
      ; expected_output : Sexp.t option
      }
    [@@deriving sexp_of]
  end
  in
  let bugs =
    List.filter_map change_tests ~f:(fun (program, input, expected_output) ->
      let syntax = Syntax.Change.t_of_sexp (Sexp.of_string program) in
      let output = Semantics.change syntax (Sexp.of_string input) in
      let expected_output = Option.map expected_output ~f:Sexp.of_string in
      if [%compare.equal: Sexp.t option] output expected_output
      then None
      else Some { Bug.program; syntax; input; output; expected_output })
  in
  if not (List.is_empty bugs)
  then failwiths "bugs in Semantics.change" bugs [%sexp_of: Bug.t list]
;;

let%expect_test _ =
  let run query inputs =
    let syntax = Syntax.Query.t_of_sexp (Sexp.of_string query) in
    List.iter inputs ~f:(fun input ->
      let result =
        Sexp.of_string input |> Semantics.query syntax |> Lazy_list.to_list |> Sexp.List
      in
      print_s result)
  in
  let examples =
    [ {|((id "(a b c)") (value 1))|}
    ; {|((id "results: (a b c)") (value 1))|}
    ; {|((id "A"      ) (value 1))|}
    ; {|((id "("      ) (value 1))|}
    ; {|((id " "      ) (value 1))|}
    ]
  in
  run {|(field id)|} examples;
  [%expect
    {|
    ("(a b c)")
    ("results: (a b c)")
    (A)
    ("(")
    (" ")
    |}];
  run {|(pipe (field id) restructure)|} examples;
  [%expect
    {|
    ((a b c))
    (results: (a b c))
    (A)
    ()
    ()
    |}]
;;

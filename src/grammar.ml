open Core
open Sexp_app
module S = Syntax

let atom x = S.Template.Atom x

let qcmds =
  let etc = S.Quote (atom "...") in
  let t = S.Quote (atom "xxx") in
  let r = Re2.create_exn "R" in
  let temp n = atom (sprintf "T[%d]" n) in
  let sexp = Sexp.Atom "SEXP" in
  let num = 999 in
  List.map
    ~f:S.Query.sexp_of_t
    [ S.Index num
    ; S.Field "FIELDNAME"
    ; S.Each
    ; S.Smash
    ; S.pipe [ t; t; etc ]
    ; S.cat [ t; t; etc ]
    ; S.This
    ; S.Die
    ; S.Atomic
    ; S.Length
    ; S.Variant ("TAG", None)
    ; S.Variant ("TAG", Some num)
    ; S.Equals (Sexp_app.Sexps.of_list [ sexp ])
    ; S.Equals (Sexp_app.Sexps.of_list [ sexp; sexp; Sexp.Atom "..." ])
    ; S.Regex r
    ; S.Test t
    ; S.Not t
    ; S.and_ [ t; t; etc ]
    ; S.or_ [ t; t; etc ]
    ; S.If (t, t, t)
    ; S.Branch (t, t, t)
    ; S.Wrap t
    ; S.Quote (temp 0)
    ; S.Change S.Id
    ; S.Restructure
    ]
;;

let ccmds =
  let t = S.Query S.This in
  let etc = S.Query (S.Quote (atom "...")) in
  List.map
    ~f:S.Change.sexp_of_t
    [ S.Rewrite (atom "P", atom "P")
    ; S.seq [ t; t; etc ]
    ; S.alt [ t; t; etc ]
    ; S.Record String.Map.empty
    ; S.Id
    ; S.Fail
    ; S.Delete
    ; S.const (Sexp.Atom "SEXP")
    ; S.try_ t
    ; S.Children t
    ; S.Topdown t
    ; S.Bottomup t
    ; S.Lowercase
    ; S.Concat
    ; S.Query (S.Quote (atom "xxx"))
    ]
;;

let cprgm x = S.Change.t_of_sexp (Sexp.of_string x)
let qprgm x = S.Query.t_of_sexp (Sexp.of_string x)

let munge =
  let change =
    cprgm
      {|
          (seq
            (topdown (try lowercase))
            (topdown (
              try (
                alt
                (rewrite 999  NUM)
                (rewrite t[0] T[0])
                (rewrite p    P)
                (rewrite r    R)
                (rewrite (record ()) (record (FIELDNAME [OPTIONS] C) ...))
                (rewrite fieldname FIELDNAME)
                (rewrite (quote xxx)  Q)
                (rewrite (quote ...)  ...)
                (rewrite (query this) C)
                (rewrite (query (quote ...)) ...)
                (rewrite (rewrite $X sexp) (const SEXP))
                (rewrite ($cmd ($_ $A $B) $C) ($cmd $A $B $C))
                (rewrite (variant $tag ()) (variant $tag))
                (rewrite (variant $tag ($num)) (variant $tag $num))
                (rewrite (change $_) (change C))
                (rewrite (query  $_) (query  Q))
                (rewrite (alt $C id) (try $C))))))
        |}
  in
  fun sexp ->
    match Semantics.change change sexp with
    | None -> assert false
    | Some sexp -> sexp
;;

let make_lead x ~print_string =
  let flag = ref true in
  fun () ->
    let str =
      if !flag
      then (
        flag := false;
        "  " ^ x ^ " ::= ")
      else "      | "
    in
    print_string str
;;

let grammar_for_readme () =
  let buf = Buffer.create 0 in
  let print_string s = Buffer.add_string buf s in
  let print_endline s =
    Buffer.add_string buf s;
    Buffer.add_string buf "\n"
  in
  print_string "=== Grammar summary for query expressions ===\n";
  print_string "See '-grammar' for a more complete grammar.\n";
  print_string "See 'sexp pat-query' for simpler regular-expression-like language.\n\n";
  let lead = make_lead "Q" ~print_string in
  List.iter qcmds ~f:(fun cmd ->
    lead ();
    print_string (Sexp.to_string_hum (munge cmd));
    print_endline "");
  Buffer.contents buf
;;

let print () =
  (print_string "\n--- grammar for query expressions ---\n\n";
   let lead = make_lead "Q" ~print_string in
   List.iter qcmds ~f:(fun cmd ->
     lead ();
     Sexp.output_hum stdout (munge cmd);
     print_endline ""));
  (print_string "\n--- grammar for change expressions ---\n\n";
   let lead = make_lead "C" ~print_string in
   List.iter ccmds ~f:(fun cmd ->
     lead ();
     Sexp.output_hum stdout (munge cmd);
     print_endline ""));
  print_string "\n--- grammar for patterns ---\n\n";
  print_endline "  P ::= ATOM";
  print_endline "      | (P ... P)";
  print_endline "      | $VAR";
  print_endline "      | @VAR";
  print_string "\n--- grammar for templates ---\n\n";
  print_endline "  T[0] ::= ATOM";
  print_endline "         | (T[0] ... T[0])";
  print_endline "         | (quote T[1])";
  print_endline "         | (unquote Q)";
  print_endline "         | (splice Q)";
  print_endline "";
  print_endline "  T[n+1] ::= ATOM";
  print_endline "           | (T[n+1] ... T[n+1])";
  print_endline "           | (quote T[n+2])";
  print_endline "           | (unquote T[n])";
  print_endline "           | (splice T[n])";
  print_endline ""
;;

let commands () =
  let munge = Semantics.query (qprgm "(or (index 0) this)") in
  List.map qcmds ~f:(fun cmd ->
    match Lazy_list.to_list (munge cmd) with
    | [ Sexp.Atom x ] -> x
    | _ -> failwith ("error: " ^ Sexp.to_string cmd))
;;

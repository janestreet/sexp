%{
open Core
module Q = Query
%}

%token WHITESPACE
%token UNNAMEDCAPTURE
%token <string> NUMBERCAPTURE
%token <string> FIELDCAPTURE
%token <string> ATOM
%token <string> ATOM_REGEX
%token DOT
%token EQUAL

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LSQUARE
%token RSQUARE

%token TWODOTS
%token STAR
%token QUESTION
%token BANG
%token SEMI
%token PLUS
%token STARPLUS
%token QUESTIONPLUS
%token PLUSPLUS

%token AND_
%token OR_

%token EOF

%nonassoc BANG
%nonassoc CURLY_BANG

%start parse
%type < Query.t > parse

%%

parse:
| Spc Query EOF { $2 }

Epsilon : { }

Spc:
| Epsilon {}
| Spc WHITESPACE {}

Query_base_wrapped:
| LPAREN Spc RPAREN { Q.List (Q.Sequence []) }
| LSQUARE Spc RSQUARE { Q.Sequence [] }
| LCURLY Spc RCURLY { Q.Set [] }
| LPAREN Spc Query RPAREN { Q.List $3 }
| LSQUARE Spc Query RSQUARE { $3 }
| LCURLY Spc Curly_queries RCURLY { Q.Set $3 }

Query_base_no_bang:
| Query_base_wrapped { $1 }
| ATOM { Atom $1 }
| ATOM_REGEX { Atom_regex $1 }
| DOT { Any }

Query_base:
| Query_base_no_bang { $1 }
| BANG Query_base_no_bang %prec BANG { Q.First_match_only $2 }

Query_capture_no_bang:
| Query_base_no_bang { $1 }
| NUMBERCAPTURE { Q.Capture_to_number (Int.of_string $1,Any) }
| FIELDCAPTURE { Q.Capture_to_name ($1,Any) }
| UNNAMEDCAPTURE Query_base_no_bang { Q.Capture_unlabeled $2 }

Query_capture:
| Query_base { $1 }
| NUMBERCAPTURE { Q.Capture_to_number (Int.of_string $1,Any) }
| FIELDCAPTURE { Q.Capture_to_name ($1,Any) }
| UNNAMEDCAPTURE Query_base { Q.Capture_unlabeled $2 }

Query_equals_capture_no_bang:
| Query_capture_no_bang { $1 }
| NUMBERCAPTURE EQUAL Query_capture_no_bang { Q.Capture_to_number (Int.of_string $1, $3) }
| FIELDCAPTURE EQUAL Query_capture_no_bang { Q.Capture_to_name ($1, $3) }

Query_equals_capture:
| Query_capture { $1 }
| NUMBERCAPTURE EQUAL Query_capture { Q.Capture_to_number (Int.of_string $1, $3) }
| FIELDCAPTURE EQUAL Query_capture { Q.Capture_to_name ($1, $3) }

Query_term_within_curly:
| Query_equals_capture_no_bang { $1 }
| Query_term_within_curly STAR { Q.Star $1 }
| Query_term_within_curly PLUS { Q.Plus $1 }
| Query_term_within_curly STARPLUS { Q.Star_greedy $1 }
| Query_term_within_curly PLUSPLUS { Q.Plus_greedy $1 }

Query_term_no_space:
| Query_equals_capture { $1 }
| Query_term_no_space STAR { Q.Star $1 }
| Query_term_no_space PLUS { Q.Plus $1 }
| Query_term_no_space QUESTION { Q.Maybe $1 }
| Query_term_no_space STARPLUS { Q.Star_greedy $1 }
| Query_term_no_space PLUSPLUS { Q.Plus_greedy $1 }
| Query_term_no_space QUESTIONPLUS { Q.Maybe_greedy $1 }

Query_term:
| Query_term_no_space Spc { $1 }

Query_sequence:
| Query_term { $1 }
| Query_term Query_sequence {
  let q1 = $1 in
  let q2 = $2 in
  match q2 with
  | Q.Sequence list -> Q.Sequence (q1 :: list)
  | _ -> Q.Sequence [ q1; q2 ]
}

Query_dots:
| Query_sequence { $1 }
| TWODOTS Spc Query_dots { Q.Subsearch $3 }
| Query_sequence TWODOTS Spc Query_dots { Q.Sequence [ $1; Q.Subsearch $4] }

Query_and:
| Query_dots { $1 }
| Query_dots AND_ Spc Query_and {
  let q1 = $1 in
  let q2 = $4 in
  match q2 with
  | Q.And list -> Q.And (q1 :: list)
  | _ -> Q.And [ q1; q2 ]
}

Query_or:
| Query_and { $1 }
| Query_and OR_ Spc Query_or {
  let q1 = $1 in
  let q2 = $4 in
  match q2 with
  | Q.Or_all list -> Q.Or_all (q1 :: list)
  | _ -> Q.Or_all [ q1; q2 ]
}

Query:
| Query_or { $1 }

Curly_item:
| BANG Query_term_within_curly QUESTION Spc %prec CURLY_BANG { ($2, {Q.Set_kind.optional=true; first_only=true}) }
| BANG Query_term_within_curly Spc %prec CURLY_BANG { ($2, {Q.Set_kind.optional=false; first_only=true}) }
| Query_term_within_curly QUESTION Spc { ($1, {Q.Set_kind.optional=true; first_only=false}) }
| Query_term_within_curly Spc { ($1, {Q.Set_kind.optional=false; first_only=false}) }

Curly_queries:
| TWODOTS Spc Query_dots { [ Q.Subsearch $3, {optional=false; first_only=false}] }
| Curly_item { [ $1 ] }
| Curly_item Curly_queries { $1 :: $2 }

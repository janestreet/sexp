{
open Core
open Stdlib.Lexing

module Token = struct
  type t = Internal_parser.token =
    | WHITESPACE

    | UNNAMEDCAPTURE
    | NUMBERCAPTURE of string
    | FIELDCAPTURE of string
    | ATOM of string
    | ATOM_REGEX of string
    | DOT
    | EQUAL

    | LPAREN
    | RPAREN
    | LCURLY
    | RCURLY
    | LSQUARE
    | RSQUARE

    | TWODOTS
    | STAR
    | QUESTION
    | BANG
    | SEMI
    | PLUS
    | STARPLUS
    | QUESTIONPLUS
    | PLUSPLUS

    | AND_
    | OR_

    | EOF
  [@@deriving sexp, variants]
end

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let lf = '\010'

let ascii_0 = Stdlib.Char.code '0'
let ascii_a = Stdlib.Char.code 'a'
let ascii_A = Stdlib.Char.code 'A'

let dec_code c1 c2 c3 =
   100 * (Stdlib.Char.code c1 - ascii_0)
  + 10 * (Stdlib.Char.code c2 - ascii_0)
  +      (Stdlib.Char.code c3 - ascii_0)

let hex_offset = function
  | 'a' .. 'f' -> ascii_a - 10
  | 'A' .. 'F' -> ascii_A - 10
  | _          -> ascii_0

let hex_code c1 c2 =
  let v1 = Stdlib.Char.code c1 - hex_offset c1 in
  let v2 = Stdlib.Char.code c2 - hex_offset c2 in
  16 * v1 + v2

let found_newline ({ lex_curr_p; _ } as lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

(* same length computation as in [Lexing.lexeme] *)
let lexeme_len { lex_start_pos; lex_curr_pos; _ } = lex_curr_pos - lex_start_pos

}

let lf = '\010'
let dos_newline = "\013\010"
let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']
let alphanumericunderscore = digit | ['a'-'z' 'A'-'Z'] | '_'
let alpha = ['a'-'z' 'A'-'Z']

let atomchars_nodot =
  alphanumericunderscore | '-' | ':' | '~' | '\'' | '`' | '#' | '$' | '^' | ','

let atomchars =
  atomchars_nodot | '.'

rule next_token = parse
| [' ' '\t']+  { Token.WHITESPACE }
| "\r\n" | "\n\r" { Lexing.new_line lexbuf; Token.WHITESPACE }
| "\n" | "\r"     { Lexing.new_line lexbuf; Token.WHITESPACE }
| '\"' {
    let buf = Buffer.create 32 in
    let pos = Lexing.lexeme_start_p lexbuf in
    let start = lexbuf.Lexing.lex_start_p in
    scan_string buf pos lexbuf;
    lexbuf.Lexing.lex_start_p <- start;
    Token.ATOM (Buffer.contents buf)
  }

| '/' {
    let buf = Buffer.create 32 in
    let pos = Lexing.lexeme_start_p lexbuf in
    let start = lexbuf.Lexing.lex_start_p in
    scan_regex buf pos lexbuf;
    lexbuf.Lexing.lex_start_p <- start;
    Token.ATOM_REGEX (Buffer.contents buf)
}

| '%' (digit digit* as str) { Token.NUMBERCAPTURE str }
| '%' (atomchars_nodot atomchars* as str) { Token.FIELDCAPTURE str }
| '%' (atomchars_nodot atomchars atomchars* as str) { Token.FIELDCAPTURE str }
| '%' (atomchars atomchars_nodot atomchars* as str) { Token.FIELDCAPTURE str }
| '%' (atomchars atomchars atomchars atomchars* as str) { Token.FIELDCAPTURE str }
| '%' { Token.UNNAMEDCAPTURE }

| (atomchars_nodot atomchars*) as str { Token.ATOM str }
| (atomchars_nodot atomchars atomchars*) as str { Token.ATOM str }
| (atomchars atomchars_nodot atomchars*) as str { Token.ATOM str }
| (atomchars atomchars atomchars atomchars*) as str { Token.ATOM str }

| "&"   { Token.AND_ }
| "|"   { Token.OR_ }
| "*"   { Token.STAR }
| "?+"  { Token.QUESTIONPLUS }
| "*+"  { Token.STARPLUS }
| "++"  { Token.PLUSPLUS }
| "?"   { Token.QUESTION }
| "+"   { Token.PLUS }
| "!"   { Token.BANG }
| ";"   { Token.SEMI }
| ".."  { Token.TWODOTS }
| "."   { Token.DOT }
| "="   { Token.EQUAL }

| '('    { Token.LPAREN }
| ')'    { Token.RPAREN }
| '{'    { Token.LCURLY }
| '}'    { Token.RCURLY }
| '['    { Token.LSQUARE }
| ']'    { Token.RSQUARE }

| eof    { Token.EOF }

and scan_regex buf start = parse
| "\\" (_ as c)  { Buffer.add_char buf c; scan_regex buf start lexbuf }
| eof     {
    failwithf "Error parsing unterminated regex at line %d char %d"
      start.pos_lnum (start.pos_cnum - start.pos_bol) ()
  }
| '/'    { () }
| _      { Buffer.add_string buf (Lexing.lexeme lexbuf); scan_regex buf start lexbuf }


(* Mostly copied from sexplib's parsing for quoted strings *)
and scan_string buf start = parse
  | '"' { () }
  | '\\' lf [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 2 in
        found_newline lexbuf len;
        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 3 in
        found_newline lexbuf len;
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_end_p lexbuf in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos_lnum (pos_cnum - pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Buffer.add_char buf (Stdlib.Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Stdlib.Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf start lexbuf
      }
  | lf
      {
        found_newline lexbuf 0;
        Buffer.add_char buf lf;
        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_subbytes buf lexbuf.lex_buffer ~pos:ofs ~len;
        scan_string buf start lexbuf
      }
  | eof
      {
        let msg =
          sprintf
            "Match.Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      }


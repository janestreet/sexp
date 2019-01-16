open Core

module Pos = struct
  type t =
    { linenum : int
    ; charnum : int
    ; line : string option
    }
  [@@deriving sexp]

  let create code lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let lines = String.split code ~on:'\n' in
    { linenum = pos.Lexing.pos_lnum
    ; charnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
    ; line = List.nth lines (pos.Lexing.pos_lnum - 1)
    }
  ;;

  let location_str t = sprintf "line %d char %d" t.linenum t.charnum
  let line t = Option.value t.line ~default:""
end

let parse_exn str =
  let lexbuf = Lexing.from_string str in
  let query =
    try Internal_parser.parse Lexer.next_token lexbuf with
    | Parsing.Parse_error ->
      let pos = Pos.create str lexbuf in
      failwithf
        "Parsing match query failed at %s in query %s"
        (Pos.location_str pos)
        (Pos.line pos)
        ()
    | exn ->
      let pos = Pos.create str lexbuf in
      Exn.reraisef
        exn
        "Parsing match query failed at %s in query %s"
        (Pos.location_str pos)
        (Pos.line pos)
        ()
  in
  query
;;

{
(* See http://tools.ietf.org/html/rfc4180 for more information. *)

open Core
module Pad = String_pad

module T = struct
  type t = Field of string | Comma | Newline
end

include T

}

let newline = [ '\r' '\n' ]
let other = [^ '\r' '\n' '"' ',']

rule read = parse
  | newline+          { Some Newline }
  | ','               { Some Comma }
  | '"'               { escaped Pad.empty lexbuf }
  | other* as field   { Some (Field field) }
  | eof               { None }

and escaped pad = parse
  | '"' '"'           { escaped (Pad.add_char pad '"') lexbuf }
  | '"'               { Some (Field (Pad.dump pad)) }
  | _ as c            { escaped (Pad.add_char pad c) lexbuf }
  | eof               { failwith "unterminated \"-delimited field" }

{

let lf = '\n'

let meta_character c ~sep =
  Char.equal c sep || Char.equal c '"' || Char.equal c lf

let output_field out x ~sep =
  if not (String.exists x ~f:(meta_character ~sep)) then
    Out_channel.output_string out x
  else begin
    let putc c = Out_channel.output_char out c in
    putc '"';
    String.iter x ~f:(fun c ->
      if Char.equal c '"' then
        (putc c; putc c)
      else
        putc c
    );
    putc '"';
  end

let write ?(sep=',') out = function
  | Field field -> output_field out field ~sep
  | Comma -> Out_channel.output_char out sep
  | Newline -> Out_channel.output_char out lf

}

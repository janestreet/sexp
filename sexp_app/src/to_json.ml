open! Core

(** [parse_json_value] attempts to parse a json value from the sexps on the right hand
    side of the flatten command. (i.e. the sexps in a Parts.t) We're guaranteed that these
    sexps are not nested. *)

module El = struct
  type t = Sexplib.Path.el =
    | Pos of int
    | Match of string * int
    | Rec of string
  [@@deriving compare]
end

(* When converting a sexp to JSON, we want to convert things that look like numbers to
   _valid_ JSON, and, critically, we also don't want to lose any information. (Idempotent
   sexp -> json -> sexp round-tripping seems like a desirable property.) It turns out this
   is rather tricky.

   OCaml and JSON have different grammars for numbers. The set of valid OCaml number
   literals is a strict superset of valid JSON number literals. Examples (not guaranteed
   to be exhaustive) of features supported in OCaml, but not in JSON:
   - Hex, octal, and binary literals, e.g., 0xc0de, 0o1357, or 0b01001010
   - Leading '+', e.g., +10
   - Multiple leading '0's, e.g., 007
   - Underscores for readability, e.g., 8_675_309

   In order to avoid losing information, even if something looks like a number in OCaml,
   we shouldn't convert it as a similar looking string that would also parse to the same
   number (e.g., 007 -> 7). This is bad because not everything that looks like a number is
   a number. Some examples include:
   - Any sort of number/account id that has a standard format, e.g. 12 digit bank account
     numbers may start with a 0.
   - Command line arguments may start with a '+' or '-' for specifying relative ranges
     rather than absolute values

   Therefore, in order to make sure we produce valid JSON, we will only convert atoms that
   are already valid JSON numbers according to its spec [1], which we'll do by testing the
   string against a straightforward regex, and we will serialize those strings without any
   modifications...

   ... with one exception: since we very frequently use underscores for readability in
   larger numbers, we will convert strings-that-look-like-numbers-with-underscores to JSON
   numbers by stripping out the underscores.

   OCaml is very generous in that it accepts any number of underscores anywhere in a
   number, e.g., "_-__1_.__2_e__+_3_4" is valid floating point literal. Conceivably, we
   could be less accepting than OCaml, and only support single occurrences of an
   underscore _between_ numbers, so things like "1_000" and "1234_5678" would still be
   converted to numbers, but not things like "_1", "2_", and "3__4". (Only allowing
   underscores every only every 3 numbers, something checked by ppx_js_style, is possible
   too, but it would make the regex more complicated, and isn't even correct; other
   cultures/situations don't always put separators as thousands/milli separators.) There's
   also a standard of using a double underscore to signify the "decimal point" in an
   integer that really represents a fixed point number. Ultimately there's too much gray
   area here, so we'll simply accept underscores anywhere like OCaml does.

   [1] : https://www.json.org/json-en.html
*)

module Assemble_to_json = struct
  type intermediate_result =
    | Array of Jsonaf.t list
    | Object of (string * Jsonaf.t) list

  let to_json = function
    | Array a -> `Array a
    | Object a -> `Object a
  ;;

  let combine acc item =
    match acc, item with
    | Array a, Array b -> Array (a @ b)
    | Object a, Object b -> Object (a @ b)
    | _ -> failwith "bug: shouldn't be dealing with both lists and records at once"
  ;;
end

(* regex based on diagram on https://www.json.org/json-en.html, but also with support for
   underscores *)
let json_number_with_underscores_re =
  Re2.create_exn
    ~options:{ Re2.Options.default with never_capture = true }
    ("^"
     (* start of string *)
     ^ "(_*-)?"
     (* negative numbers start with '-' *)
     ^ "(_*0_*"
     (* integer part is either a 0 *)
     ^ "|_*[1-9](\\d|_)*)"
     (* or 1-9 followed by any digits *)
     ^ "(\\._*\\d(\\d|_)*)?"
     (* decimal part, '.' then at least one digit, is optional *)
     ^ "(_*[eE]_*[-+]?_*\\d(\\d|_)*)?"
     (* exponent part, also optional *)
     ^ "$" (* end of string *))
;;

let parse_json_value : Sexp.t -> Jsonaf.t = function
  | Sexp.Atom s ->
    (match Bool.of_string s with
     | true -> `True
     | false -> `False
     | exception _ ->
       if Re2.matches json_number_with_underscores_re s
       then `Number (String.filter s ~f:(fun c -> not (Char.equal c '_')))
       else `String s)
  | Sexp.List [] -> `Null
  | Sexp.List _ -> failwith "bug - every value after flatten should be an atom or nil"
;;

let one_deeper l =
  List.map l ~f:(fun (p, v) ->
    match p with
    | Sexplib.Path.Pos _ :: p -> p, v
    | Rec _ :: p -> p, v
    | _ -> assert false)
;;

let group_by_path l =
  List.group l ~break:(fun (p1, _) (p2, _) ->
    not ([%compare.equal: El.t option] (List.hd p1) (List.hd p2)))
;;

let rec assemble_to_json (l : (Sexplib.Path.t * Sexp.t) list) =
  match group_by_path l with
  | [ [ (p, v) ] ] -> assemble_to_json1 p v
  | [] -> assert false
  | groups ->
    let parts =
      List.map groups ~f:(fun l ->
        match List.hd_exn l with
        | Pos _ :: _, _ -> Assemble_to_json.Array [ assemble_to_json (one_deeper l) ]
        | Rec n :: _, _ -> Assemble_to_json.Object [ n, assemble_to_json (one_deeper l) ]
        | _ -> assert false)
    in
    List.reduce_balanced_exn parts ~f:Assemble_to_json.combine |> Assemble_to_json.to_json

and assemble_to_json1 p v : Jsonaf.t =
  match p with
  | [] -> parse_json_value v
  | Pos _ :: p -> `Array [ assemble_to_json1 p v ]
  | Rec n :: p -> `Object [ n, assemble_to_json1 p v ]
  | Match _ :: _ -> assert false
;;

let json_of_sexp sexp =
  let parts = Parts.flatten sexp in
  assemble_to_json parts
;;

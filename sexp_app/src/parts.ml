open Core

module Path = struct
  include Sexplib.Path

  module X : sig
    val compare_el : el -> el -> int
  end = struct
    type nonrec el = el =
      | Pos of int
      | Match of string * int
      | Rec of string
    [@@deriving compare]
  end

  include X

  let to_string l =
    match l with
    | [] -> "."
    | l ->
      String.concat
        ~sep:""
        (List.map l ~f:(function
          | Pos p -> sprintf ".[%i]" p
          | Rec n -> sprintf ".%s" n
          | Match (n, p) -> sprintf ".%s[%i]" n p))
  ;;
end

open Path
open Sexplib.Type

type t = (Path.t * Sexp.t) list

let identifier = Or_error.ok_exn (Re2.create "^[a-zA-Z_][_a-zA-Z0-9]+$")

let is_record l =
  List.for_all l ~f:(function
    | List [ Atom n; _ ] -> Re2.matches identifier n
    | _ -> false)
  && Option.is_none
       (List.find_a_dup
          (List.rev_map l ~f:(function
            | List [ Atom n; _ ] -> n
            | _ -> assert false))
          ~compare:Poly.compare)
;;

let rec flatten path t =
  match t with
  | Atom s -> [ List.rev path, Atom s ]
  | List [] -> [ List.rev path, List [] ]
  | List l ->
    if is_record l
    then
      List.concat
        (List.map l ~f:(function
          | List [ Atom n; v ] -> flatten (Rec n :: path) v
          | _ -> assert false))
    else List.concat (List.mapi l ~f:(fun p e -> flatten (Pos p :: path) e))
;;

let flatten t = flatten [] t

let rec assemble (l : (Path.t * Sexp.t) list) =
  let group l =
    List.group l ~break:(fun (p1, _) (p2, _) ->
      not ([%compare.equal: Path.el option] (List.hd p1) (List.hd p2)))
  in
  let one_deeper l =
    List.map l ~f:(fun (p, v) ->
      match p with
      | Pos _ :: p -> p, v
      | Rec _ :: p -> p, v
      | _ -> assert false)
  in
  match group l with
  | [ [ (p, v) ] ] -> assemble1 p v
  | [] -> assert false
  | groups ->
    List
      (List.map groups ~f:(fun l ->
         match List.hd_exn l with
         | Pos _ :: _, _ -> assemble (one_deeper l)
         | Rec n :: _, _ -> List [ Atom n; assemble (one_deeper l) ]
         | _ -> assert false))

and assemble1 p v =
  match p with
  | [] -> v
  | Pos _ :: p -> List [ assemble1 p v ]
  | Rec n :: p -> List [ List [ Atom n; assemble1 p v ] ]
  | Match _ :: _ -> assert false
;;

let output t out =
  List.iter t ~f:(fun (p, v) ->
    Printf.fprintf out "%s\t%s\n" (Path.to_string p) (Sexp.to_string_mach v));
  Out_channel.output_string out "\n"
;;

let input inch =
  let rec loop res =
    match In_channel.input_line ~fix_win_eol:true inch with
    | None | Some "" -> List.rev res
    | Some l ->
      let a, b = String.lsplit2_exn l ~on:'\t' in
      let path = Sexplib.Path.parse a in
      loop ((path, Sexp.of_string b) :: res)
  in
  loop []
;;

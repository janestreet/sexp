(** [parse_json_value] attempts to parse a json value from the sexps on the right
    hand side of the flatten command. (i.e. the sexps in a Parts.t)
    We're guaranteed that these sexps are not nested.
*)

open Core
open Sexplib
open Composition_infix

module El = struct
  type t = Sexplib.Path.el =
    | Pos of int
    | Match of string * int
    | Rec of string
  [@@deriving compare]
end

let parse_json_value : Sexp.t -> Jsonaf.t =
  let try_to_parse f s =
    try Some (f s) with
    | _ -> None
  in
  function
  | Sexp.Atom s ->
    (match try_to_parse Bool.of_string s with
     | Some true -> `True
     | Some false -> `False
     | None ->
       (match try_to_parse Float.of_string s with
        | Some _ -> `Number s
        | None -> `String s))
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

module Assemble_to_json = struct
  type intermediate_result =
    | None
    | Array of Jsonaf.t list
    | Object of (string * Jsonaf.t) list

  let to_json = function
    | None -> failwith "bug: there should be at least one group"
    | Array a -> `Array a
    | Object a -> `Object a
  ;;

  let combine acc item =
    match acc, item with
    | None, item -> item
    | Array a, Array b -> Array (a @ b)
    | Object a, Object b -> Object (a @ b)
    | _ -> failwith "bug: shouldn't be dealing with both lists and records at once"
  ;;
end

let rec assemble_to_json (l : (Path.t * Sexp.t) list) =
  match group_by_path l with
  | [ [ (p, v) ] ] -> assemble_to_json1 p v
  | [] -> assert false
  | groups ->
    let parts =
      List.map groups ~f:(fun l ->
        match List.hd_exn l with
        | Pos _ :: _, _ -> Assemble_to_json.Array [ assemble_to_json (one_deeper l) ]
        | Rec n :: _, _ ->
          Assemble_to_json.Object [ n, assemble_to_json (one_deeper l) ]
        | _ -> assert false)
    in
    List.fold parts ~init:Assemble_to_json.None ~f:Assemble_to_json.combine
    |> Assemble_to_json.to_json

and assemble_to_json1 p v : Jsonaf.t =
  match p with
  | [] -> parse_json_value v
  | Pos _ :: p -> `Array [ assemble_to_json1 p v ]
  | Rec n :: p -> `Object [ n, assemble_to_json1 p v ]
  | Match _ :: _ -> assert false
;;

let to_json () =
  match
    let%map.List sexp = Sexp.input_sexps In_channel.stdin in
    let parts = Sexp_app.Parts.flatten sexp in
    let json = assemble_to_json parts in
    print_endline (Jsonaf.to_string_hum json)
  with
  | (_ : unit list) -> ()
  | exception End_of_file -> ()
;;

let of_json () =
  let module Conv = Sexplib.Conv in
  let rec convert : Jsonaf.t -> Sexp.t = function
    | `Null -> Conv.sexp_of_unit ()
    | `True -> Conv.sexp_of_bool true
    | `False -> Conv.sexp_of_bool false
    | `Number s -> Conv.sexp_of_string s
    | `String s -> Conv.sexp_of_string s
    | `Array json_list -> Conv.sexp_of_list convert json_list
    | `Object alist ->
      let sexp_of_item = Conv.sexp_of_pair Conv.sexp_of_string convert in
      Conv.sexp_of_list sexp_of_item alist
  in
  let parser_state =
    (* Print as we go so the incremental work can be gc'd  *)
    Angstrom.lift (convert >> print_s) Jsonaf.Parser.t
    |> Angstrom.many
    |> Angstrom.map ~f:(fun (_ : unit list) -> ())
    |> Angstrom.Buffered.parse
  in
  let parser_state =
    In_channel.fold_lines ~init:parser_state In_channel.stdin ~f:(fun parser_state line ->
      Angstrom.Buffered.feed parser_state (`String line))
  in
  let parser_state = Angstrom.Buffered.feed parser_state `Eof in
  match Angstrom.Buffered.state_to_result parser_state with
  | Error s -> eprint_s [%message "Got an error parsing the json" s]
  | Ok () -> ()
;;

let to_json_command =
  Command.basic
    ~summary:"Convert sexps on stdin to json"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> to_json ())
;;

let of_json_command =
  Command.basic
    ~summary:"Convert json on stdin to sexps"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> of_json ())
;;

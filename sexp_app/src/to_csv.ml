open Core

include struct
  module Command = Command
  module Extended_list = List
end

let contains xs x =
  try
    ignore (List.find_exn xs ~f:(fun y -> String.equal x y));
    true
  with
  | _ -> false
;;

let rec has_duplicates = function
  | [] -> None
  | x :: xs -> if contains xs x then Some x else has_duplicates xs
;;

module Query : sig
  val is_a_record : Sexp.t -> bool

  val header : Sexp.t -> string list

  val record : view_atoms_as_strings:bool -> string list -> Sexp.t -> Csv_record.t
end = struct
  open Syntax

  let is_a_record =
    let pair = and_ [ Index 0; Index 1; Not (Index 2) ] in
    let q =
      and_ [ Not Atomic; pipe [ Each; and_ [ pair; Test (pipe [ Index 0; Atomic ]) ] ] ]
    in
    fun sexp ->
      match Lazy_list.decons (Semantics.query q sexp) with
      | None -> false
      | Some _ -> true
  ;;

  let header =
    let q = pipe [ Each; Index 0 ] in
    let strip = function
      | Sexp.Atom x -> x
      | Sexp.List _ -> failwith "failed to ensure that header is called on a record"
    in
    fun sexp ->
      let hs = Lazy_list.to_list (Lazy_list.map ~f:strip (Semantics.query q sexp)) in
      match has_duplicates hs with
      | Some x -> failwith ("duplicate field name '" ^ x ^ "'")
      | None -> hs
  ;;

  let record ~view_atoms_as_strings fields =
    let q = cat (List.map fields ~f:(fun f -> Wrap (Field f))) in
    let coerce (f, results) =
      match results with
      | Sexp.List [ x ] ->
        (match x with
         | Sexp.Atom str when view_atoms_as_strings -> str
         | _ -> Sexp.to_string x)
      | Sexp.List (x :: _) ->
        eprintf "multiple values for field %s. Arbitrarily picking the first one.\n" f;
        Sexp.to_string x
      | _ ->
        eprintf "missing value for field %s\n" f;
        ""
    in
    fun sexp ->
      List.map
        ~f:coerce
        (List.zip_exn fields (Lazy_list.to_list (Semantics.query q sexp)))
  ;;
end

let csv_of_sexp ~view_atoms_as_strings sexps =
  match Lazy_list.decons sexps with
  | None -> Lazy_list.empty ()
  | Some (x, _) ->
    if not (Query.is_a_record x) then failwith "first element is not a record\n";
    let header = Query.header x in
    let extract = Query.record ~view_atoms_as_strings header in
    Lazy_list.cons header (Lazy_list.map ~f:extract sexps)
;;

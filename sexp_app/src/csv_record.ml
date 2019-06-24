open Core
open Csv_lexeme.T

type t = string list

let read lexbuf =
  let fields = Manifest.create () in
  let return () = Some (Manifest.to_list fields) in
  let rec loop prev =
    match Csv_lexeme.read lexbuf with
    | None ->
      (match prev with
       | Newline -> None
       | _ -> return ())
    | Some this ->
      (match prev, this with
       | Newline, Field x | Comma, Field x ->
         Manifest.add fields x;
         loop this
       | Field x, Field y -> failwithf "adjacent fields (%s) and (%s)" x y ()
       | Field _, Comma -> loop this
       | Field _, Newline -> return ()
       | Newline, Newline -> loop this
       | Newline, Comma | Comma, Comma ->
         Manifest.add fields "";
         loop Comma
       | Comma, Newline ->
         Manifest.add fields "";
         return ())
  in
  loop Newline
;;

let write ?(sep = ',') out t =
  let lexemes =
    List.fold_right t ~init:[ Newline ] ~f:(fun field lexemes ->
      match lexemes with
      | [ Newline ] -> Field field :: lexemes
      | lexemes -> Field field :: Comma :: lexemes)
  in
  List.iter lexemes ~f:(fun lexeme -> Csv_lexeme.write out lexeme ~sep)
;;

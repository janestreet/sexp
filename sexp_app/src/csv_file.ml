(** CSV files *)

open Core

type t = Csv_record.t Lazy_list.t

let test_length ~found ~expected =
  if found <> expected
  then failwithf "length mismatch: expected %i but found %i" expected found ()
;;

let read lexbuf =
  Lazy_list.build ~seed:None ~f:(fun len_opt ->
    match Csv_record.read lexbuf with
    | None -> None
    | Some record ->
      let found = List.length record in
      (match len_opt with
       | None -> Some (record, Some found)
       | Some expected ->
         test_length ~found ~expected;
         Some (record, len_opt)))
;;

let write ?(sep = ',') out t =
  let length = ref None in
  Lazy_list.iter t ~f:(fun record ->
    let found = List.length record in
    (match !length with
     | None -> length := Some found
     | Some expected -> test_length ~found ~expected);
    Csv_record.write out record ~sep)
;;

open! Core

type t =
  | Atom of string
  | Capture of string
  | List of t list

let escape_percents =
  unstage (String.Escaping.escape ~escapeworthy:[ '%' ] ~escape_char:'\\')
;;

let unescape_percents = unstage (String.Escaping.unescape ~escape_char:'\\')

let rec t_of_sexp sexp =
  match (sexp : Sexp.t) with
  | Atom s ->
    if String.is_prefix s ~prefix:"%"
    then Capture (String.chop_prefix_exn s ~prefix:"%")
    else Atom (unescape_percents s)
  | List list -> List (List.map list ~f:t_of_sexp)
;;

let rec sexp_of_t t : Sexp.t =
  match t with
  | Atom s -> Atom (escape_percents s)
  | Capture s -> Atom ("%" ^ s)
  | List list -> List (List.map list ~f:sexp_of_t)
;;

let rec all_captures_aux t acc =
  match t with
  | Atom _ -> acc
  | Capture s -> s :: acc
  | List list -> List.fold_right list ~init:acc ~f:all_captures_aux
;;

let all_captures t = all_captures_aux t []

let rec embed_captures t ~f : Sexp.t =
  match t with
  | Atom s -> Atom s
  | Capture s -> f s
  | List list -> List (List.map list ~f:(embed_captures ~f))
;;

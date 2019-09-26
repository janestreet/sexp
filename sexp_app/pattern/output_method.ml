open! Core

module Format = struct
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

  let ts_of_string s = Sexp.scan_sexps (Lexing.from_string s) |> List.map ~f:t_of_sexp

  let rec all_captures_aux t acc =
    match t with
    | Atom _ -> acc
    | Capture s -> s :: acc
    | List list -> List.fold_right list ~init:acc ~f:all_captures_aux
  ;;

  let all_captures t = all_captures_aux t []

  let rec embed_captures t ~f : Sexp.t list =
    match t with
    | Atom s -> [ Atom s ]
    | Capture s -> f s
    | List list -> [ List (List.concat_map list ~f:(embed_captures ~f)) ]
  ;;
end

module Wrap_mode = struct
  type 'query_result t =
    | Wrap_always : Sexp.t t
    | Wrap_non_singletons : Sexp.t t
    | Unwrap_always : Sexp.t list t
  [@@deriving sexp_of]

  type some_wrap_mode = T : _ t -> some_wrap_mode
end

type 'query_result t =
  | Formats : _ Wrap_mode.t * Format.t list -> Sexp.t list t
  | List : 'query_result Wrap_mode.t -> Sexp.t t
  | Record : 'query_result Wrap_mode.t -> Sexp.t t
  | Single_capture : 'query_result Wrap_mode.t -> 'query_result t
  | Map : Sexp.t list String.Map.t t (** Return a map from capture name to captures *)
[@@deriving sexp_of]

type some_output_method = T : _ t -> some_output_method

let default_method query ~wrap_mode =
  let { Query.Capture_count.num_number_captures
      ; num_named_captures
      ; num_unlabeled_captures
      }
    =
    Query.count_captures query
  in
  if num_named_captures > 0
  then T (Record wrap_mode)
  else if num_number_captures > 0 || num_unlabeled_captures > 1
  then T (List wrap_mode)
  else if num_unlabeled_captures = 1
  then T (Single_capture wrap_mode)
  else failwith "No captures % were specified in pattern"
;;

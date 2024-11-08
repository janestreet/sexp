open Core

let simple_query query sexp = Lazy_list.to_list (Semantics.query query sexp)

let get_fields sexp field =
  simple_query (Syntax.Pipe (Syntax.Smash, Syntax.Field field)) sexp
;;

module Non_unique_field = struct
  type t =
    { field : string
    ; sexp : Sexp.t
    ; matches : Sexp.t list
    }
  [@@deriving sexp]
end

let get_one_field sexp field =
  let results = get_fields sexp field in
  match results with
  | [] | _ :: _ :: _ ->
    Or_error.error
      "non-unique field"
      { Non_unique_field.field; sexp; matches = results }
      Non_unique_field.sexp_of_t
  | [ result ] -> Ok result
;;

let sexp_rewrite_aux sexp ~f:visit =
  let rec aux sexp =
    match visit sexp with
    | `Changed sexp' -> Some sexp'
    | `Removed -> None
    | `Unchanged ->
      (match sexp with
       | Sexp.Atom _ -> Some sexp
       | Sexp.List sexps ->
         let sexps' = List.filter_map ~f:aux sexps in
         if List.length sexps = List.length sexps'
            && List.for_all2_exn ~f:phys_equal sexps sexps'
         then Some sexp
         else Some (Sexp.List sexps'))
  in
  match aux sexp with
  | None -> Or_error.error "not a record" sexp Fn.id
  | Some sexp -> Ok sexp
;;

let sexp_rewrite sexp ~f = sexp_rewrite_aux sexp ~f |> Or_error.ok_exn

let immediate_fields = function
  | Sexp.List children ->
    List.fold ~init:(Ok []) children ~f:(fun acc child ->
      match acc with
      | Error _ -> acc
      | Ok by_field ->
        (match child with
         | Sexp.List [ Sexp.Atom field; value ] ->
           (match List.Assoc.find by_field ~equal:String.equal field with
            | None -> Ok (List.Assoc.add by_field ~equal:String.equal field value)
            | Some _ -> Or_error.error "multiple values for field" field String.sexp_of_t)
         | _ -> Or_error.error "not a field" child Fn.id))
    |> fun result ->
    (* Restore original order *)
    Or_error.map result ~f:List.rev
  | Sexp.Atom atom -> Or_error.error "not a record" atom String.sexp_of_t
;;

let to_record_sexp by_fields =
  Sexp.List
    (List.map by_fields ~f:(fun (field, value) -> Sexp.List [ Sexp.Atom field; value ]))
;;

let replace_immediate_field ~field ~value sexp =
  Or_error.map (immediate_fields sexp) ~f:(fun by_field ->
    List.Assoc.remove by_field ~equal:String.equal field
    |> (fun by_field -> List.Assoc.add by_field ~equal:String.equal field value)
    |> to_record_sexp)
;;

let replace_field_recursively ~field ~value sexp =
  sexp_rewrite_aux sexp ~f:(function
    | Sexp.List [ Sexp.Atom f; _ ] when String.equal field f ->
      `Changed (Sexp.List [ Sexp.Atom f; value ])
    | _ -> `Unchanged)
;;

let replace_field ~field ~value sexp immediate_or_recursive =
  match immediate_or_recursive with
  | `Immediate -> replace_immediate_field ~field ~value sexp
  | `Recursive ->
    let%bind.Or_error result = replace_field_recursively ~field ~value sexp in
    if Sexp.( = ) result sexp
    then Or_error.error "field not found" field String.sexp_of_t
    else Ok result
;;

let remove_immediate_field ~field sexp =
  Or_error.map (immediate_fields sexp) ~f:(fun by_field ->
    List.Assoc.remove by_field ~equal:String.equal field |> to_record_sexp)
;;

let remove_field_recursively ~field sexp =
  sexp_rewrite_aux sexp ~f:(function
    | Sexp.List [ Sexp.Atom f; _ ] when String.equal field f -> `Removed
    | _ -> `Unchanged)
;;

let remove_field ~field sexp immediate_or_recursive =
  match immediate_or_recursive with
  | `Immediate -> remove_immediate_field ~field sexp
  | `Recursive -> remove_field_recursively ~field sexp
;;

module%test Utils = struct
  let sexp =
    Sexp.of_string "((first (a b c)) (second 123) (third ()) (fourth ((foo a) (boo b))))"
  ;;

  let%test _ =
    [%compare.equal: Sexp.t Or_error.t]
      (get_one_field sexp "second")
      (Ok (Sexp.Atom "123"))
  ;;

  let%test _ = Result.is_error (get_one_field sexp "zoo")

  let%test _ =
    [%compare.equal: Sexp.t Or_error.t] (get_one_field sexp "boo") (Ok (Sexp.Atom "b"))
  ;;

  let%test _ = Result.is_error (immediate_fields (Sexp.of_string "zoo"))
  let%test _ = Result.is_error (immediate_fields (Sexp.of_string "(zoo)"))
  let%test _ = Result.is_error (immediate_fields (Sexp.of_string "(zoo boo)"))
  let%test _ = Result.is_error (immediate_fields (Sexp.of_string "((good true)(bad))"))

  let%test _ =
    [%equal: Sexp.t]
      (List.Assoc.find_exn
         (Or_error.ok_exn (immediate_fields sexp))
         ~equal:String.equal
         "second")
      (Atom "123")
  ;;

  let%test _ =
    [%equal: Sexp.t]
      (List.Assoc.find_exn
         (Or_error.ok_exn (immediate_fields sexp))
         ~equal:String.equal
         "third")
      (List [])
  ;;

  let%test _ =
    [%equal: Sexp.t]
      (List.Assoc.find_exn
         (Or_error.ok_exn (immediate_fields sexp))
         ~equal:String.equal
         "fourth")
      (Sexp.of_string "((foo a) (boo b))")
  ;;

  let%test _ =
    [%equal: Sexp.t] (to_record_sexp (Or_error.ok_exn (immediate_fields sexp))) sexp
  ;;

  let%test _ =
    let value = Sexp.Atom "my-new-value" in
    let sexp = Or_error.ok_exn (replace_field ~field:"second" ~value sexp `Immediate) in
    [%equal: Sexp.t]
      (List.Assoc.find_exn
         (Or_error.ok_exn (immediate_fields sexp))
         ~equal:String.equal
         "second")
      value
  ;;

  let to_alist_exn sexp = Or_error.ok_exn (immediate_fields sexp)

  let ( -@! ) record_sexp field_name =
    List.Assoc.find_exn (to_alist_exn record_sexp) ~equal:String.equal field_name
  ;;

  let ( -@? ) record_sexp field_name =
    List.Assoc.find (to_alist_exn record_sexp) ~equal:String.equal field_name
  ;;

  let%test _ =
    let value = Sexp.Atom "my-new-value" in
    let sexp = Or_error.ok_exn (replace_field ~field:"foo" ~value sexp `Recursive) in
    [%equal: Sexp.t] (sexp -@! "fourth" -@! "foo") value
  ;;

  let%test "remove_field immediate" =
    let sexp = Or_error.ok_exn (remove_field ~field:"second" sexp `Immediate) in
    [%equal: Sexp.t option] (sexp -@? "second") None
  ;;

  let%test "remove_field recursive" =
    let sexp = Or_error.ok_exn (remove_field ~field:"foo" sexp `Recursive) in
    [%equal: Sexp.t option] (sexp -@! "fourth" -@? "foo") None
  ;;

  let%test "remove_field error" =
    let sexp_or_error = remove_field ~field:"foo" (Sexp.of_string "(foo)") `Immediate in
    Or_error.is_error sexp_or_error
  ;;
end

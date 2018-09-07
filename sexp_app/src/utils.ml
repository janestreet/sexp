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
  | []
  | _ :: _ :: _ ->
    Or_error.error
      "non-unique field"
      { Non_unique_field.field; sexp; matches = results }
      Non_unique_field.sexp_of_t
  | [ result ] -> Ok result
;;

let sexp_rewrite sexp ~f:visit =
  let rec aux sexp =
    match visit sexp with
    | `Changed sexp' -> sexp'
    | `Unchanged ->
      (match sexp with
       | Sexp.Atom _ -> sexp
       | Sexp.List sexps ->
         let sexps' = List.map ~f:aux sexps in
         if List.for_all2_exn ~f:phys_equal sexps sexps' then sexp else Sexp.List sexps')
  in
  aux sexp
;;

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
  sexp_rewrite sexp ~f:(function
    | Sexp.List [ Sexp.Atom f; _ ]
      when String.equal field f -> `Changed (Sexp.List [ Sexp.Atom f; value ])
    | _ -> `Unchanged)
;;

let replace_field ~field ~value sexp immediate_or_recursive =
  match immediate_or_recursive with
  | `Immediate -> replace_immediate_field ~field ~value sexp
  | `Recursive ->
    let result = replace_field_recursively ~field ~value sexp in
    if result = sexp
    then Or_error.error "field not found" field String.sexp_of_t
    else Ok result
;;

let%test_module "Utils" =
  (module struct
    let sexp =
      Sexp.of_string
        "((first (a b c)) (second 123) (third ()) (fourth ((foo a) (boo b))))"
    ;;

    let%test _ = get_one_field sexp "second" = Ok (Sexp.Atom "123")
    let%test _ = Result.is_error (get_one_field sexp "zoo")
    let%test _ = get_one_field sexp "boo" = Ok (Sexp.Atom "b")
    let%test _ = Result.is_error (immediate_fields (Sexp.of_string "zoo"))
    let%test _ = Result.is_error (immediate_fields (Sexp.of_string "(zoo)"))
    let%test _ = Result.is_error (immediate_fields (Sexp.of_string "(zoo boo)"))
    let%test _ = Result.is_error (immediate_fields (Sexp.of_string "((good true)(bad))"))

    let%test _ =
      List.Assoc.find_exn
        (Or_error.ok_exn (immediate_fields sexp))
        ~equal:String.equal
        "second"
      = Sexp.Atom "123"
    ;;

    let%test _ =
      List.Assoc.find_exn
        (Or_error.ok_exn (immediate_fields sexp))
        ~equal:String.equal
        "third"
      = Sexp.List []
    ;;

    let%test _ =
      List.Assoc.find_exn
        (Or_error.ok_exn (immediate_fields sexp))
        ~equal:String.equal
        "fourth"
      = Sexp.of_string "((foo a) (boo b))"
    ;;

    let%test _ = to_record_sexp (Or_error.ok_exn (immediate_fields sexp)) = sexp

    let%test _ =
      let value = Sexp.Atom "my-new-value" in
      let sexp =
        Or_error.ok_exn (replace_field ~field:"second" ~value sexp `Immediate)
      in
      List.Assoc.find_exn
        (Or_error.ok_exn (immediate_fields sexp))
        ~equal:String.equal
        "second"
      = value
    ;;

    let%test _ =
      let value = Sexp.Atom "my-new-value" in
      let sexp = Or_error.ok_exn (replace_field ~field:"foo" ~value sexp `Recursive) in
      let fourth_value =
        List.Assoc.find_exn
          (Or_error.ok_exn (immediate_fields sexp))
          ~equal:String.equal
          "fourth"
      in
      List.Assoc.find_exn
        (Or_error.ok_exn (immediate_fields fourth_value))
        ~equal:String.equal
        "foo"
      = value
    ;;
  end)
;;

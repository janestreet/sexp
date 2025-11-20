open Core
open Async

(* Helper for error messages. *)
let quoted flag = "\"" ^ flag ^ "\""

(* Command does some automatic handling of exceptions and prints out errors as sexps. When
   combined with the quoting behavior above, this leads to lots of ugly escaping. We
   provide our own custom error function that just prints out the message and immediately
   exits to avoid this. *)
let cmd_error msg =
  Core.prerr_endline msg;
  Core.exit 1
;;

module If_no_key = struct
  module T = struct
    type t =
      | Drop
      | Fail
      | Unit
      | Force_unit
      | Wrap
    [@@deriving sexp, enumerate, equal]

    let to_string t =
      sexp_of_t t
      |> Sexp.to_string
      |> String.lowercase
      |> String.tr ~target:'_' ~replacement:'-'
    ;;
  end

  include T

  let flag =
    let open Command.Param in
    flag "if-no-key" (optional_with_default Fail (Arg_type.enumerated (module T))) ~doc:""
  ;;

  let from_modifiers modifiers ~flag_and_arg =
    match modifiers with
    | None -> None
    | Some modifiers ->
      let if_no_key = ref None in
      List.iter modifiers ~f:(function
        | "mfail" -> if_no_key := Some Fail
        | "mdrop" -> if_no_key := Some Drop
        | "munit" -> if_no_key := Some Unit
        | "mforceunit" -> if_no_key := Some Force_unit
        | "mwrap" -> if_no_key := Some Wrap
        | unknown ->
          cmd_error
            ("Unknown missing key behavior modifier "
             ^ quoted unknown
             ^ " in "
             ^ flag_and_arg));
      !if_no_key
  ;;
end

module Sexp = struct
  include Sexp
  include Hashable.Make (Sexp)
end

module Entry = struct
  type t =
    { first_line : int
    ; values : Sexp.t list
    }
end

type t =
  { mutable line : int
  ; entries : Entry.t Sexp.Table.t
  }

module Group = struct
  type t =
    { key : Sexp.t
    ; count : int
    ; values : Sexp.t list
    }
  [@@deriving sexp]
end

module Extraction_result = struct
  type 'a t =
    | Ok of 'a
    | Error of string
    | Drop

  let map t ~f =
    match t with
    | Ok x -> Ok (f x)
    | Error s -> Error s
    | Drop -> Drop
  ;;
end

let sexp_unit = Sexp.List []

(* Given a list of [Key_extractor]s and corresponding [If_no_key] behaviors, return a
   function that returns an [Sexp.t Extraction_result.t].

   If a single extractor is passed in, the returned function will return exactly what the
   extractor extracted. If multiple extractors are passed in, the extracted value will be
   a [Sexp.List] where each element is the value extracted by the corresponding extractor
   (so the return type will always be [Sexp.t] as opposed to [Sexp.t list]). *)
let create_extractor ~keys ~default_if_no_key =
  (* Small helper that takes in a [Key_extractor.t], builds the extract function, then
     maps the regular [(Sexp.t, Extraction_error.t * string) result] into an
     [Sexp.t Extraction_result.t]. *)
  let make_extraction_result_extractor (key_extractor, if_no_key) =
    let if_no_key = Option.value if_no_key ~default:default_if_no_key in
    let extract = Key_extractor.extract_or_error_fn key_extractor |> unstage in
    stage (fun input_sexp ->
      match extract input_sexp with
      | Ok key ->
        (match if_no_key with
         | If_no_key.Wrap -> Extraction_result.Ok (Sexp.List [ key ])
         | Drop | Fail | Force_unit -> Ok key
         | Unit ->
           (match key with
            | Sexp.List [] ->
              Extraction_result.Error
                "Ambiguous unit error: missing keys set to be converted to unit, but \
                 input contains an actual unit.\n\
                 Either specify \"-if-no-key wrap\" (or \"/mwrap\") to wrap all keys in \
                 a list (so missing keys are \"()\", and actual unit keys are \"(())\"), \
                 or specify \"-if-no-key force-unit\" (or \"/mforceunit\") to forcibly \
                 convert all missing keys to units."
            | _ -> Ok key))
      | Error (Key_extractor.Extraction_error.Missing_key, s) ->
        (match if_no_key with
         | If_no_key.Drop -> Extraction_result.Drop
         | Fail -> Error s
         | Unit | Force_unit | Wrap -> Extraction_result.Ok sexp_unit)
      | Error (_, s) -> Error s)
  in
  match keys with
  | [] -> cmd_error "At least one key must be specified"
  | [ key ] -> make_extraction_result_extractor key
  | _ ->
    let extract_fns =
      List.map keys ~f:make_extraction_result_extractor |> List.map ~f:unstage
    in
    stage (fun input_sexp ->
      List.fold extract_fns ~init:(Extraction_result.Ok []) ~f:(fun acc extract ->
        match acc, extract input_sexp with
        | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Drop, _ -> Drop
        | _, Drop -> Drop
        | Ok keys, Ok k -> Ok (k :: keys))
      |> Extraction_result.map ~f:(fun keys -> Sexp.List (List.rev keys)))
;;

let create () : t = { line = 0; entries = Sexp.Table.create () }

let add_exn (t : t) ~extract sexp =
  t.line <- t.line + 1;
  match extract sexp with
  | Extraction_result.Ok key ->
    Hashtbl.change t.entries key ~f:(function
      | None -> Some { Entry.first_line = t.line; values = [ sexp ] }
      | Some { first_line; values } -> Some { first_line; values = sexp :: values })
  | Drop -> ()
  | Error s -> cmd_error ("Error on sexp " ^ Int.to_string t.line ^ ": " ^ s)
;;

let groups (t : t) =
  Hashtbl.to_alist t.entries
  |> List.map ~f:(fun (key, { first_line; values }) ->
    let count = List.length values in
    (* The values will be accumulated in reverse order; we need to reverse them here so
       they appear in the same order as the input. *)
    first_line, { Group.key; count; values = List.rev values })
  |> List.sort ~compare:(fun (line1, _) (line2, _) -> Int.compare line1 line2)
  |> List.map ~f:snd
;;

let main ~keys ~default_if_no_key =
  let t = create () in
  let extract = create_extractor ~keys ~default_if_no_key |> unstage in
  let%bind () =
    Reader.read_sexps (Lazy.force Reader.stdin)
    |> Pipe.iter_without_pushback ~f:(add_exn t ~extract)
  in
  groups t
  |> Deferred.List.iter ~how:`Sequential ~f:(fun group ->
    [%sexp_of: Group.t] group |> Core.print_s;
    return ())
;;

let field_doc = "FIELD Group by the value associated with this field"
let index_doc = "INDEX Group by the value at this index in the top-level of a sexp"

let query_doc =
  "QUERY Group by the values referenced by this query, as used in sexp query"
;;

let pat_query_doc =
  "PATTERN Group by the values reference by this query, as used in sexp pat-query"
;;

let get_doc = "PATH Group by the values referenced by this path, as used in sexp get"

let select_doc =
  "PATH Group by the values referenced by this path, as used in sexp select"
;;

let general_doc = "KEY Group by the key"

let with_no_missing_key_behavior key_extractors_param =
  let%map_open.Command key_extractors = key_extractors_param in
  Option.map key_extractors ~f:(fun key_extractors ->
    List.map key_extractors ~f:(fun x -> x, None))
;;

let command =
  let readme () =
    String.strip
      {|
Groups a list of s-expressions. The group key can be specified in a variety of formats:
a field name, a query as used for sexp query, a pattern as used for sexp pat-query,
a path as used for sexp get, or a CSS-style program as used for sexp select.

For each unique key in the input, one record-style sexp will be outputted with three
keys: "key", containing the common key for the grouped values; "values", containing
the list of grouped values; and "count", the number of grouped values.

The output groups will be sorted in order of appearance of each key, and the list of
values for each key will be in the same order as in the input.

You can group by multiple keys, in which case the "key" in the output will be a list
containing each individual key. You can specify multiple keys by passing the same
selector format multiple times, e.g., "-field foo -field bar", or, if you want to use
multiple kinds of selectors, by using the special "-key" flag (see below).

Missing keys can be handled in various ways by using the "-if-no-key" flag. By default
input sexps with missing keys will cause an error. By passing "-if-no-key drop", inputs
with missing keys will be dropped from the input. Missing keys can also just be treated
as units ( "()" ) by passing "-if-no-key unit".

When using "-if-no-key unit", there is no way to disambiguate between a missing key
and an actual unit. If an actual unit is seen in the input, an error will be thrown.
To resolve this, there are two options:
  - "-if-no-key wrap" will wrap all keys in an outer sexp, so that actual unit keys
    become "(())", while missing keys are "()".
  - "-if-no-key force-unit" will simply treat all missing keys as units, even if some
    keys are actual units.

To specify multiple keys that use different access formats, or if you want to specify
different handling of missing values for each key, use the "-key" flag and pass a
string of the form "<access_kind><modifiers>:<arg>", where:
  - <access_kind> is one of (field|index|query|pat-query|get|select)
  - <modifiers> are optional strings to control specific sort behavior for that column.
    Each modifier is prefixed with a '/'. Options are:
      mfail  -> raise an error if the key is missing (default)
      mdrop  -> drop sexps with missing keys from the output
      munit  -> treat missing keys as unit ( "()" )
      mwrap  -> wrap all keys in an outer sexp; missing keys are units ( "()" )
      mforceunit -> treat missing keys as unit; even if there are actual unit keys
  - <arg> is the arg you would pass to the equivalent "-<access_kind>" flag.
      |}
  in
  Command.async_or_error
    ~behave_nicely_in_pipeline:true
    ~summary:"group input sexps by an arbitrary key"
    ~readme
    (let%map_open.Command default_if_no_key = If_no_key.flag
     and keys =
       choose_one
         ~if_nothing_chosen:Raise
         [ Key_extractor.field_param ~doc:field_doc () |> with_no_missing_key_behavior
         ; Key_extractor.index_param ~doc:index_doc () |> with_no_missing_key_behavior
         ; Key_extractor.query_param ~doc:query_doc () |> with_no_missing_key_behavior
         ; Key_extractor.pat_query_param ~doc:pat_query_doc ()
           |> with_no_missing_key_behavior
         ; Key_extractor.get_param ~doc:get_doc () |> with_no_missing_key_behavior
         ; Key_extractor.select_param ~doc:select_doc () |> with_no_missing_key_behavior
         ; Key_extractor.general_param
             ~doc:general_doc
             ~modifiers:(Map If_no_key.from_modifiers)
             ()
         ]
     in
     fun () ->
       Deferred.Or_error.try_with ~extract_exn:true (fun () ->
         main ~default_if_no_key ~keys))
;;

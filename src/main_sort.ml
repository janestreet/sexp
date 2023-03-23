open Core
open Async
open Sexp_app
module Sexp_path = Sexplib.Path

(* We want to include the original flag (name + value) in our error messages.
   Flag values often include whitespace, so they need to be quoted. [Sys.quote]
   will add single quotes, but then to separate the provided flag from the
   rest of the error message, we will wrap it in double quotes. We won't escape
   any double quotes inside the flag value because 1) this won't happen that
   often, 2) the error message isn't getting interpreted so it's fine to not
   be escaped, and 3) because it makes it harder to read. *)
let rebuild_flag name value = name ^ " " ^ Sys.quote value
let quoted flag = "\"" ^ flag ^ "\""

(* Command does some automatic handling of exceptions and prints out errors
   as sexps. When combined with the quoting behavior above, this leads to lots
   of ugly escaping. We provide our own custom error function that just prints
   out the message and immediately exits to avoid this. *)
let cmd_error msg =
  Core.prerr_endline msg;
  Core.exit 1
;;

let cmd_error_s msg sexp =
  Core.prerr_endline (msg ^ "\n  " ^ Sexp.to_string_hum ~indent:2 sexp);
  Core.exit 1
;;

(* Comparison behavior can be configured in four different ways:
   - string vs numeric comparisons
   - for string comparisons, whether we use natural sort ("z2" < "z10")
   - for string comparisons, whether the comparison is case sensitive
   - whether the comparison should be reversed

   Note that specifying the string behaviors (natural/case-sensitivity)
   is incompatible with specifying numeric sort.

   We allow configuration of this behavior using top-level flags
   (-numeric, -natural, -ignore-case, and -reverse respectively). We
   also allow specifying them on a per-key basis using the special
   '-key "field/asc/rev:foo"' flags.

   The top-level flags will specify the default behavior for each key.
   When using the "-key ..." syntax with behavior modifiers (the part
   that looks like '/rev/i'), that will override that default behavior.

   We will check for the clashing numeric/string flags in the top-level
   default flags (so you cannot specify both '-numeric' and '-natural),
   and in the modifiers for any individual key (so you cannot specify
   '/num/nat), but we won't check inconsistencies in "merging" of them.
   That is, you can specify "-natural" sort as the top-level default,
   but then still specify "/num" as a modifier for a specific key.
*)
module Compare_behavior = struct
  type t =
    { numeric : bool option
    ; natural : bool option
    ; case_insensitive : bool option
    ; reverse : bool option
    }

  let default =
    { numeric = None; natural = None; case_insensitive = None; reverse = None }
  ;;

  let numeric t = Option.value t.numeric ~default:false
  let natural t = Option.value t.natural ~default:false
  let case_insensitive t = Option.value t.case_insensitive ~default:false
  let reverse t = Option.value t.reverse ~default:false

  let from_top_level_flags ~numeric ~natural ~case_insensitive ~reverse =
    if numeric && case_insensitive
    then cmd_error "Cannot specify both \"-numeric\" and \"-ignore-case\""
    else if numeric && natural
    then cmd_error "Cannot specify both \"-numeric\" and \"-natural\"";
    let none_if_false b = if b then Some b else None in
    { numeric = none_if_false numeric
    ; natural = none_if_false natural
    ; case_insensitive = none_if_false case_insensitive
    ; reverse = none_if_false reverse
    }
  ;;

  let from_modifiers modifiers flag =
    let numeric = ref None in
    let natural = ref None in
    let case_insensitive = ref None in
    let reverse = ref None in
    List.iter modifiers ~f:(function
      | "asc" -> reverse := Some false
      | "desc" -> reverse := Some true
      | "rev" -> reverse := Some true
      | "nat" -> natural := Some true
      | "str" -> numeric := Some false
      | "num" -> numeric := Some true
      | "s" -> case_insensitive := Some false
      | "i" -> case_insensitive := Some true
      | unknown ->
        cmd_error
          ("Unknown sort behavior modifier " ^ quoted unknown ^ " in " ^ quoted flag));
    (* These checks are nearly the same as the ones above in in from_top_level_flags,
       but we print out slightly different error messages, so duplicate the logic here. *)
    if Option.value !numeric ~default:false
    then
      if Option.is_some !case_insensitive
      then
        cmd_error ("Cannot specify both \"/num\" and \"/s\" or \"/i\" in " ^ quoted flag)
      else if Option.value !natural ~default:false
      then cmd_error ("Cannot specify both \"/num\" and \"/nat\" in " ^ quoted flag);
    { numeric = !numeric
    ; natural = !natural
    ; case_insensitive = !case_insensitive
    ; reverse = !reverse
    }
  ;;

  (* [snd] trumps [fst] *)
  let option_merge fst snd = if Option.is_some snd then snd else fst

  let merge sk1 sk2 =
    { numeric = option_merge sk1.numeric sk2.numeric
    ; natural = option_merge sk1.natural sk2.natural
    ; case_insensitive = option_merge sk1.case_insensitive sk2.case_insensitive
    ; reverse = option_merge sk1.reverse sk2.reverse
    }
  ;;
end

module Key_extractor = struct
  type t =
    | Identity
    | Query of
        { query : Syntax.Query.t
        ; flag : string
        }
    | Pat_query of
        { pat_query : Sexp_app_pattern.Query.t
        ; flag : string
        }
    | Select of
        { program : string
        ; flag : string
        }

  let build_field_extractor ~field ~flag = Query { query = Field field; flag }
  let build_index_extractor ~index ~flag = Query { query = Index index; flag }

  let build_query_extractor ~query ~flag =
    (* A little clunky vs. just using parse_string_exn, but we prioritize providing a
       little more context to the error message, which may be helpful if using sexp sort
       in a long pipeline. *)
    let query_sexps =
      match Parsexp.Many.parse_string query with
      | Ok x -> x
      | Error e ->
        cmd_error_s
          ("Invalid sexp query [QUERY] passed as " ^ quoted flag)
          [%sexp (e : Parsexp.Parse_error.t)]
    in
    Query { query = Syntax.pipe (List.map ~f:Syntax.Query.t_of_sexp query_sexps); flag }
  ;;

  let build_pat_query_extractor ~pat_query ~flag =
    let pat_query = Sexp_app_pattern.Parser.parse_exn pat_query in
    let ({ num_named_captures; num_number_captures; num_unlabeled_captures }
         : Sexp_app_pattern.Query.Capture_count.t)
      =
      Sexp_app_pattern.Query.count_captures pat_query
    in
    if num_named_captures > 0
    then
      cmd_error
        ("Can't define a sort key using labeled captures in pat-query [PATTERN]: "
         ^ quoted flag);
    if num_number_captures = 0 && num_unlabeled_captures = 0
    then
      cmd_error ("pat-query [PATTERN] must specify at least one capture: " ^ quoted flag);
    Pat_query { pat_query; flag }
  ;;

  (* a little compiler from the sort of path expressions you would pass to [sexp get] to the
     sort of query you would pass to [sexp query] *)
  let sexp_query_expression_of_get_query_string path flag =
    let path_parts =
      try Sexp_path.parse path with
      | exn ->
        cmd_error_s ("Bad sexp get [PATH] passed as " ^ quoted flag) [%sexp (exn : Exn.t)]
    in
    let query_sexps =
      List.concat_map path_parts ~f:(function
        | Pos n -> [ Syntax.Index n ]
        | Match (tag, n) -> Syntax.[ Test (Variant (tag, None)); Index (n + 1) ]
        | Rec name -> [ Syntax.Field name ])
    in
    Syntax.pipe query_sexps
  ;;

  let build_get_extractor ~path ~flag =
    Query { query = sexp_query_expression_of_get_query_string path flag; flag }
  ;;

  let build_select_extractor ~program ~flag = Select { program; flag }

  let extract_fn = function
    | Identity -> fun sexp -> [ sexp ]
    | Query { query; _ } -> fun sexp -> Lazy_list.to_list (Semantics.query query sexp)
    | Pat_query { pat_query; _ } -> unstage (Pat_query.run pat_query)
    | Select { program; _ } -> unstage (Sexp_select.select_staged program)
  ;;

  let flag = function
    | Identity -> None
    | Query { flag; _ } | Pat_query { flag; _ } | Select { flag; _ } -> Some flag
  ;;

  (* Returns a projection function that extracts a key from a sexp and applies a
     transformation function. Will error if no key is extracted from the sexp, or if
     multiple keys are extracted. Will also bubble up an error returned by the
     transformation function. *)
  let build_extract_or_error_fn t ~transform =
    let extract = extract_fn t in
    let specified_by_flag =
      match flag t with
      | None -> ""
      | Some flag -> " specified by " ^ quoted flag
    in
    let project sexp =
      match extract sexp with
      | [] -> Error ("Missing key" ^ specified_by_flag)
      | [ key ] ->
        (match transform key with
         | Ok key -> Ok key
         | Error msg -> Error ("Key" ^ specified_by_flag ^ " " ^ msg))
      | _ -> Error ("Key" ^ specified_by_flag ^ " occurs multiple times")
    in
    stage project
  ;;
end

module type Sort_key = sig
  type key

  val extract : Sexp.t -> (key, string) result
  val compare : key -> key -> int
end

let sexp_compare_fn ~compare_atom =
  let module My_sexp = struct
    type atom = string
    (* Because of how we've chosen the names [atom] and [compare_atom], it is as
       though we've added a [@@deriving compare] to this type declaration, but instead of
       getting the usual generic String.compare, we've swapped in our own definition. *)

    type t = Sexp.t =
      | Atom of atom
      | List of t list
    [@@deriving compare]
  end
  in
  My_sexp.compare
;;

(* There's no String_extended.Caseless.collate, so when we are doing a case-insensitive
   natural sort, we want to transform the whole key to lowercase. We do this once, when
   initially processing each sexp to generate the key. We run the risk of lowercasing a
   bunch of strings that we don't need to, but if a sub-sexp is compared even once, then
   we didn't do any extra work, and on average we make log(N) comparisons per node, so
   we'd have to do a LOT of extra work for it to be slower to do the transforms once. In
   most cases anyway we'll just be comparing a single atom, in which case the upfront
   transform is obviously the correct route. *)
let rec sexp_lowercase = function
  | Sexp.Atom s -> Sexp.Atom (String.lowercase s)
  | List l -> List (List.map l ~f:sexp_lowercase)
;;

let build_sexp_key ~key_extractor ~compare_behavior : (module Sort_key) =
  let case_insensitive = Compare_behavior.case_insensitive compare_behavior in
  let natural = Compare_behavior.natural compare_behavior in
  let reverse = Compare_behavior.reverse compare_behavior in
  let transform =
    if case_insensitive && natural
    then fun sexp -> Ok (sexp_lowercase sexp)
    else fun x -> Ok x
  in
  let extract =
    unstage (Key_extractor.build_extract_or_error_fn key_extractor ~transform)
  in
  let compare_atom =
    match case_insensitive, natural with
    | false, false -> String.compare
    | true, false -> String.Caseless.compare
    | _, true -> String_extended.collate
  in
  let compare = sexp_compare_fn ~compare_atom in
  let compare = if reverse then fun a b -> compare b a else compare in
  let module Sexp_key : Sort_key = struct
    type key = Sexp.t

    let extract = extract
    let compare = compare
  end
  in
  (module Sexp_key)
;;

let build_number_key ~key_extractor ~compare_behavior : (module Sort_key) =
  let transform sexp =
    match sexp with
    | Sexp.Atom s ->
      (try Ok (Float.of_string s) with
       | _ -> Error ("is not a number\n  > " ^ Sexp.to_string sexp))
    | _ -> Error ("is not an atom\n  > " ^ Sexp.to_string_hum sexp)
  in
  let extract =
    unstage (Key_extractor.build_extract_or_error_fn key_extractor ~transform)
  in
  let reverse = Compare_behavior.reverse compare_behavior in
  let compare = if reverse then fun a b -> Float.compare b a else Float.compare in
  let module Number_key : Sort_key = struct
    type key = float

    let extract = extract
    let compare = compare
  end
  in
  (module Number_key)
;;

module Chain_sort (A : Sort_key) (B : Sort_key) : Sort_key = struct
  type key = A.key * B.key

  let extract sexp =
    match A.extract sexp, B.extract sexp with
    | Ok a, Ok b -> Ok (a, b)
    | Error err_a, Error err_b -> Error (err_a ^ "\n" ^ err_b)
    | Error err_a, _ -> Error err_a
    | _, Error err_b -> Error err_b
  ;;

  let compare (a1, b1) (a2, b2) =
    let a_compare = A.compare a1 a2 in
    if a_compare = 0 then B.compare b1 b2 else a_compare
  ;;
end

let none_if_empty = function
  | [] -> None
  | l -> Some l
;;

let field_flag =
  let%map_open.Command fields =
    flag "field" (listed string) ~doc:"FIELD Sort by the value associated with this field"
  in
  List.map fields ~f:(fun field ->
    ( Key_extractor.build_field_extractor ~field ~flag:(rebuild_flag "-field" field)
    , Compare_behavior.default ))
  |> none_if_empty
;;

let index_flag =
  let%map_open.Command indexes =
    flag
      "index"
      (listed int)
      ~doc:"INDEX Sort by the value at this index in the top-level of a sexp"
  in
  List.map indexes ~f:(fun index ->
    ( Key_extractor.build_index_extractor
        ~index
        ~flag:(rebuild_flag "-index" (Int.to_string index))
    , Compare_behavior.default ))
  |> none_if_empty
;;

let query_flag =
  let%map_open.Command queries =
    flag
      "query"
      (listed string)
      ~doc:"QUERY Sort by the values referenced by this query, as used in sexp query"
  in
  List.map queries ~f:(fun query ->
    ( Key_extractor.build_query_extractor ~query ~flag:(rebuild_flag "-query" query)
    , Compare_behavior.default ))
  |> none_if_empty
;;

let pat_query_flag =
  let%map_open.Command pat_queries =
    flag
      "pat-query"
      (listed string)
      ~doc:"PATTERN Sort by the values reference by this query, as used in sexp pat-query"
  in
  List.map pat_queries ~f:(fun pat_query ->
    ( Key_extractor.build_pat_query_extractor
        ~pat_query
        ~flag:(rebuild_flag "-pat-query" pat_query)
    , Compare_behavior.default ))
  |> none_if_empty
;;

let get_flag =
  let%map_open.Command paths =
    flag
      "get"
      (listed string)
      ~doc:"PATH Sort by the values referenced by this path, as used in sexp get"
  in
  List.map paths ~f:(fun path ->
    ( Key_extractor.build_get_extractor ~path ~flag:(rebuild_flag "-get" path)
    , Compare_behavior.default ))
  |> none_if_empty
;;

let select_flag =
  let%map_open.Command programs =
    flag
      "select"
      (listed string)
      ~doc:"PATH Sort by the values referenced by this path, as used in sexp select"
  in
  List.map programs ~f:(fun program ->
    ( Key_extractor.build_select_extractor ~program ~flag:(rebuild_flag "-select" program)
    , Compare_behavior.default ))
  |> none_if_empty
;;

let extract_compare_behavior specifier flag =
  match String.split specifier ~on:'/' with
  | [] -> failwith "impossible"
  | [ specifier ] -> specifier, Compare_behavior.default
  | specifier :: modifiers -> specifier, Compare_behavior.from_modifiers modifiers flag
;;

let key_flag =
  let%map_open.Command keys = flag "key" (listed string) ~doc:"KEY Sort by the key" in
  List.map keys ~f:(fun key ->
    let flag = rebuild_flag "-key" key in
    match String.lsplit2 ~on:':' key with
    | None -> cmd_error ("Invalid -key format (no ':' separator): " ^ quoted flag)
    | Some (specifier, arg) ->
      let specifier, compare_behavior = extract_compare_behavior specifier flag in
      let extractor =
        match specifier with
        | "field" -> Key_extractor.build_field_extractor ~field:arg ~flag
        | "index" ->
          let arg = Int.of_string arg in
          Key_extractor.build_index_extractor ~index:arg ~flag
        | "query" -> Key_extractor.build_query_extractor ~query:arg ~flag
        | "pat-query" -> Key_extractor.build_pat_query_extractor ~pat_query:arg ~flag
        | "get" -> Key_extractor.build_get_extractor ~path:arg ~flag
        | "select" -> Key_extractor.build_select_extractor ~program:arg ~flag
        | specifier ->
          cmd_error ("Unknown key specifier " ^ quoted specifier ^ " in " ^ quoted flag)
      in
      extractor, compare_behavior)
  |> none_if_empty
;;

let mach_flag =
  Command.Param.map Shared_params.machine ~f:(fun mach ->
    if mach then Sexp.to_string_mach else fun sexp -> Sexp.to_string_hum ?indent:None sexp)
;;

let pipe_mapi_result pipe ~f =
  let rec foldi_until_err i pipe accum =
    match%bind Pipe.read pipe with
    | `Eof -> return (Ok (List.rev accum))
    | `Ok x ->
      (match f i x with
       | Error _ as e -> return e
       | Ok y -> foldi_until_err (i + 1) pipe (y :: accum))
  in
  foldi_until_err 0 pipe []
;;

type 'a sort_key_and_sexp =
  { key : 'a
  ; sexp : Sexp.t
  }

let command =
  let readme () =
    String.strip
      {|
Sorts a list of s-expressions. The sort key be specified in a variety of formats:
a field name, a query as used for sexp query, a pattern as used for sexp pat-query,
a path as used for sexp get, or a CSS-style program as used for sexp select.

To sort by multiple keys, you have several options. If you want to use the same
kind of sort for each key, and you want to specify how to access each key in the
same way, then you can repeatedly specify a flag, e.g., `-field foo -field bar`,
or `-get .a.b -get .m.n -get .y.z`.

If you want to sort by multiple keys, and use different specifiers for each, or
if you want to use different types of sorts for multiple keys, e.g., sort one key
as a string, and another key as a number but in descending order, you have to use
the special "-key" flag:

To use the "-key" flag, pass a string of the form "<access_kind><modifiers>:<arg>",
where:
  - <access_kind> is one of (field|index|query|pat-query|get|select)
  - <modifiers> are optional strings to control specific sort behavior for that column.
    Each modifier is prefixed with a '/'. Options are:
      asc  -> sort in ascending order (default)
      desc -> sort in descending (reverse) order
      rev  -> sort in reverse order
      nat  -> use natural sort
      num  -> use numeric sort
      str  -> use string sort (default)
      s    -> use case-sensitive sort (default)
      i    -> use case-insensitive sort
  - <arg> is the arg you would pass to the equivalent "-<access_kind>" flag.

For example, if you have sexps of users with names and ages:
  ((name ((first <first_name>) (last <last_name>))) (age <age>))

then you can sort them age, oldest first, then last name using:

  sexp sort -key "field/num/desc:age" -key "get:.name.last"


A note on -reverse: -reverse inverts the result of comparisons, rather than
reversing the final output, which may lead to slightly unexpected behavior when
combined with -unique. This matches the behavior of the unix sort command.
|}
  in
  Command.async
    ~summary:"Sort a list of s-expressions"
    ~readme
    ~behave_nicely_in_pipeline:true
    (let%map_open.Command sexp_to_string = mach_flag
     and key_extractors =
       choose_one
         ~if_nothing_chosen:
           (Default_to [ Key_extractor.Identity, Compare_behavior.default ])
         [ field_flag
         ; index_flag
         ; query_flag
         ; pat_query_flag
         ; get_flag
         ; select_flag
         ; key_flag
         ]
     and reverse =
       flag
         "reverse"
         ~aliases:[ "descending" ]
         no_arg
         ~doc:" Reverse the result of the comparisons"
     and unique =
       flag
         "unique"
         no_arg
         ~doc:" Output just the first of consecutive sexps with equal keys"
     and natural =
       flag "natural" no_arg ~doc:" Use natural sort, i.e., \"z2\" comes before \"z10\""
     and case_insensitive = flag "ignore-case" no_arg ~doc:" ignore case when sorting"
     and numeric =
       flag
         "numeric"
         (* Add "n" as an explicit alias so that users don't get an "ambiguous prefix"
            error (with "-natural") when using "-n". We expect numeric sort to be much
            more commonly used. *)
         ~aliases:[ "n" ]
         no_arg
         ~doc:" Treat sort keys as numbers (keys must be atoms)"
     in
     fun () ->
       let top_level_compare_behavior =
         Compare_behavior.from_top_level_flags
           ~numeric
           ~natural
           ~case_insensitive
           ~reverse
       in
       let key_modules =
         List.map
           key_extractors
           ~f:(fun (key_extractor, key_compare_behavior) : (module Sort_key) ->
             let compare_behavior =
               Compare_behavior.merge top_level_compare_behavior key_compare_behavior
             in
             let (module Key : Sort_key) =
               if Compare_behavior.numeric compare_behavior
               then build_number_key ~key_extractor ~compare_behavior
               else build_sexp_key ~key_extractor ~compare_behavior
             in
             (module Key))
       in
       let (module Sorter : Sort_key) =
         List.reduce_exn
           key_modules
           ~f:(fun (module A : Sort_key) (module B : Sort_key) : (module Sort_key) ->
             let module Chained = Chain_sort (A) (B) in
             (module Chained))
       in
       let extract_or_error i sexp =
         match Sorter.extract sexp with
         | Ok key -> Ok { key; sexp }
         | Error msg ->
           Or_error.error_string ("Error on sexp " ^ Int.to_string (i + 1) ^ ": " ^ msg)
       in
       let sexps_in = Reader.read_sexps (Lazy.force Reader.stdin) in
       match%bind pipe_mapi_result sexps_in ~f:extract_or_error with
       | Error err ->
         prerr_endline (Error.to_string_hum err);
         exit 1
       | Ok keys_and_sexps ->
         let compare e1 e2 = Sorter.compare e1.key e2.key in
         (* We unfortunately can't use [List.dedup_and_sort] for the unique case for two
            reasons. First, it doesn't use a stable sort, and then second, it calls
            [remove_consecutive_duplicates] internally, but with the default value for
            ~which_to_keep, which is `Last, which is opposite of what we want. (We are
            matching the behavior of sort -u, which returns the first of an equal run.) *)
         let maybe_dedup =
           if unique
           then
             List.remove_consecutive_duplicates ~which_to_keep:`First ~equal:(fun e1 e2 ->
               compare e1 e2 = 0)
           else Fn.id
         in
         keys_and_sexps
         |> List.stable_sort ~compare
         |> maybe_dedup
         |> List.iter ~f:(fun e -> print_endline (sexp_to_string e.sexp));
         return ())
;;

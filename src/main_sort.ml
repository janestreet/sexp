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

(* You might think that we should be able to get away with using a plain [result] for the
   return type of [Sort_key.extract], as opposed to maybe just making the ['ok] half of
   the result an ['a option].

   Unfortunately, that approach fails when using multiple sort keys, because each sort key
   might specify a different way to handle missing values. In [Chain_sort] we need someway
   to combine the extraction result from each key in a way the would preserve our
   intention to drop a row from the output. Because of how we construct the final
   [Sort_key] module, there's no place that has a view of all the extracted keys for an
   input Sexp as a list and say, "Ah, the third key extracted a [None] and it has
   [How_to_handle_missing.Drop] configured, so we'll ignore it." *)
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

(* We support multiple different behaviors when keys are missing. The default will be to
   error, but we also support putting all the input sexps with missing keys first or last. *)
module How_to_handle_missing = struct
  module T = struct
    type t =
      | Error
      | First
      | Last
      | Drop
    [@@deriving sexp, enumerate]

    let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
  end

  include T

  let flag =
    let open Command.Param in
    flag
      "missing"
      (optional (Arg_type.enumerated (module T)))
      ~doc:"ACTION How to handle missing values"
  ;;
end

(* Comparison behavior can be configured in four different ways:
   - string vs numeric comparisons
   - for string comparisons, whether we use natural sort ("z2" < "z10")
   - for string comparisons, whether the comparison is case sensitive
   - whether the comparison should be reversed

   Note that specifying the string behaviors (natural/case-sensitivity) is incompatible
   with specifying numeric sort.

   We allow configuration of this behavior using top-level flags (-numeric, -natural,
   -ignore-case, and -reverse respectively). We also allow specifying them on a per-key
   basis using the special '-key "field/asc/rev:foo"' flags.

   The top-level flags will specify the default behavior for each key. When using the
   "-key ..." syntax with behavior modifiers (the part that looks like '/rev/i'), that
   will override that default behavior.

   We will check for the clashing numeric/string flags in the top-level default flags (so
   you cannot specify both '-numeric' and '-natural), and in the modifiers for any
   individual key (so you cannot specify '/num/nat), but we won't check inconsistencies in
   "merging" of them. That is, you can specify "-natural" sort as the top-level default,
   but then still specify "/num" as a modifier for a specific key.
*)
module Compare_behavior = struct
  type t =
    { numeric : bool option
    ; natural : bool option
    ; case_insensitive : bool option
    ; reverse : bool option
    ; handle_missing : How_to_handle_missing.t option
    }

  let default =
    { numeric = None
    ; natural = None
    ; case_insensitive = None
    ; reverse = None
    ; handle_missing = None
    }
  ;;

  let numeric t = Option.value t.numeric ~default:false
  let natural t = Option.value t.natural ~default:false
  let case_insensitive t = Option.value t.case_insensitive ~default:false
  let reverse t = Option.value t.reverse ~default:false

  let handle_missing t =
    Option.value t.handle_missing ~default:How_to_handle_missing.Error
  ;;

  let from_top_level_flags ~numeric ~natural ~case_insensitive ~reverse ~handle_missing =
    if numeric && case_insensitive
    then cmd_error "Cannot specify both \"-numeric\" and \"-ignore-case\""
    else if numeric && natural
    then cmd_error "Cannot specify both \"-numeric\" and \"-natural\"";
    let none_if_false b = if b then Some b else None in
    { numeric = none_if_false numeric
    ; natural = none_if_false natural
    ; case_insensitive = none_if_false case_insensitive
    ; reverse = none_if_false reverse
    ; handle_missing
    }
  ;;

  let from_modifiers modifiers flag_and_arg =
    let numeric = ref None in
    let natural = ref None in
    let case_insensitive = ref None in
    let reverse = ref None in
    let handle_missing = ref None in
    List.iter modifiers ~f:(function
      | "asc" -> reverse := Some false
      | "desc" -> reverse := Some true
      | "rev" -> reverse := Some true
      | "nat" -> natural := Some true
      | "str" -> numeric := Some false
      | "num" -> numeric := Some true
      | "s" -> case_insensitive := Some false
      | "i" -> case_insensitive := Some true
      | "merror" -> handle_missing := Some How_to_handle_missing.Error
      | "mfirst" -> handle_missing := Some How_to_handle_missing.First
      | "mlast" -> handle_missing := Some How_to_handle_missing.Last
      | "mdrop" -> handle_missing := Some How_to_handle_missing.Drop
      | unknown ->
        cmd_error
          ("Unknown sort behavior modifier " ^ quoted unknown ^ " in " ^ flag_and_arg));
    (* These checks are nearly the same as the ones above in in from_top_level_flags, but
       we print out slightly different error messages, so duplicate the logic here. *)
    if Option.value !numeric ~default:false
    then
      if Option.is_some !case_insensitive
      then
        cmd_error ("Cannot specify both \"/num\" and \"/s\" or \"/i\" in " ^ flag_and_arg)
      else if Option.value !natural ~default:false
      then cmd_error ("Cannot specify both \"/num\" and \"/nat\" in " ^ flag_and_arg);
    { numeric = !numeric
    ; natural = !natural
    ; case_insensitive = !case_insensitive
    ; reverse = !reverse
    ; handle_missing = !handle_missing
    }
  ;;

  (* [snd] trumps [fst] *)
  let option_merge fst snd = if Option.is_some snd then snd else fst

  let merge sk1 sk2 =
    { numeric = option_merge sk1.numeric sk2.numeric
    ; natural = option_merge sk1.natural sk2.natural
    ; case_insensitive = option_merge sk1.case_insensitive sk2.case_insensitive
    ; reverse = option_merge sk1.reverse sk2.reverse
    ; handle_missing = option_merge sk1.handle_missing sk2.handle_missing
    }
  ;;
end

module type Sort_key = sig
  type key

  val extract : Sexp.t -> key Extraction_result.t
  val compare : key -> key -> int
end

let sexp_compare_fn ~compare_atom =
  let module My_sexp = struct
    type atom = string
    (* Because of how we've chosen the names [atom] and [compare_atom], it is as though
       we've added a [@@deriving compare] to this type declaration, but instead of getting
       the usual generic String.compare, we've swapped in our own definition. *)

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

(* Once we have built the properly typed [extract], [transform] and [compare] functions,
   we can create a [Sort_key] module. The [key] type of this module will depend on how we
   handle missing values. If we handle missing keys by putting them first or last, then
   we'll have [type key = 'a option]. If missing keys result in an error, or we drop input
   sexps with missing keys, then we'll have [type key].

   In this function we wrap the [extract] and [compare] functions to implement the desired
   behavior when we see missing values.

   Note that we add additional logic when we wrap the [compare] function, so we have to
   implement the final reversal _after_ wrapping, otherwise the new compare function
   wouldn't flip the comparisons between missing keys and present keys. If we flipped the
   compare function before wrapping it to handle missing keys, this would cause "-missing
   first" to always put missing keys first, even if "-reverse" were passed in. I think
   that behavior would be reasonable too ("I passed in '-missing first'. I don't care if I
   also passed in '-reverse'. I want missing first."). Either way the behavior could be
   slightly surprising but I think having "-reverse" always flip the order of the output
   is less surprising.
*)
let combine_extract_transform_and_compare_into_sort_key_module
  (type k)
  ~(key_extractor : Key_extractor.t)
  ~(extract : Sexp.t -> (Sexp.t, Key_extractor.Extraction_error.t * string) result)
  ~(transform : Sexp.t -> (k, string) result)
  ~(compare : k -> k -> int)
  ~(handle_missing : How_to_handle_missing.t)
  ~(reverse : bool)
  =
  let maybe_reverse_compare cf = if reverse then fun a b -> cf b a else cf in
  let extractor_source = Key_extractor.extractor_source key_extractor in
  let transform sexp =
    match transform sexp with
    | Ok x -> Extraction_result.Ok x
    | Error msg -> Error ("Key" ^ extractor_source ^ " " ^ msg)
  in
  (* For [Error] and [Drop]. *)
  let make_non_option_module ~extract : (module Sort_key) =
    (module struct
      type key = k

      let extract = extract
      let compare = maybe_reverse_compare compare
    end)
  in
  let (module Sexp_key : Sort_key) =
    match handle_missing with
    | Error ->
      (* We could just keep the error as is, but we do a little extra work here to add
         helpful context to the error message we show the user. *)
      let extract sexp =
        match extract sexp with
        | Ok x -> transform x
        | Error (Key_extractor.Extraction_error.Missing_key, s) ->
          Error
            (s
             ^ "\n"
             ^ "You can use the -missing flag to declare how missing keys should be \
                handled.")
        | Error (_, s) -> Error s
      in
      make_non_option_module ~extract
    | Drop ->
      (* Convert [Missing_key] errors to [Drop]. *)
      let extract sexp =
        match extract sexp with
        | Ok x -> transform x
        | Error (Key_extractor.Extraction_error.Missing_key, _) -> Drop
        | Error (_, s) -> Error s
      in
      make_non_option_module ~extract
    | First | Last ->
      (* Wrap extracted value in a [Some], or return [None] on [Missing_key] errors. *)
      let extract sexp =
        match extract sexp with
        | Ok x -> transform x |> Extraction_result.map ~f:Option.some
        | Error (Key_extractor.Extraction_error.Missing_key, _) -> Ok None
        | Error (_, s) -> Error s
      in
      (* When we have two [Some]s we just use the normal compare function, but otherwise
         we sort [None]s (missing keys) before or after actual keys. *)
      let none_to_some, some_to_none =
        match handle_missing with
        | First -> -1, 1
        | Last -> 1, -1
        | Error | Drop -> failwith "impossible; just checked above"
      in
      let compare a b =
        match a, b with
        | Some a, Some b -> compare a b
        | None, Some _ -> none_to_some
        | Some _, None -> some_to_none
        | None, None -> 0
      in
      (module struct
        type key = k option

        let extract = extract
        let compare = maybe_reverse_compare compare
      end)
  in
  (module Sexp_key : Sort_key)
;;

let build_sexp_key ~key_extractor ~compare_behavior : (module Sort_key) =
  let case_insensitive = Compare_behavior.case_insensitive compare_behavior in
  let natural = Compare_behavior.natural compare_behavior in
  let reverse = Compare_behavior.reverse compare_behavior in
  let handle_missing = Compare_behavior.handle_missing compare_behavior in
  let transform =
    if case_insensitive && natural
    then fun sexp -> Ok (sexp_lowercase sexp)
    else fun x -> Ok x
  in
  let extract = unstage (Key_extractor.extract_or_error_fn key_extractor) in
  let compare_atom =
    match case_insensitive, natural with
    | false, false -> String.compare
    | true, false -> String.Caseless.compare
    | _, true -> String_extended.collate
  in
  let compare = sexp_compare_fn ~compare_atom in
  combine_extract_transform_and_compare_into_sort_key_module
    ~key_extractor
    ~extract
    ~transform
    ~compare
    ~handle_missing
    ~reverse
;;

let build_number_key ~key_extractor ~compare_behavior : (module Sort_key) =
  let transform sexp =
    match sexp with
    | Sexp.Atom s ->
      (try Ok (Float.of_string s) with
       | _ -> Error ("is not a number\n  > " ^ Sexp.to_string sexp))
    | _ -> Error ("is not an atom\n  > " ^ Sexp.to_string_hum sexp)
  in
  let extract = unstage (Key_extractor.extract_or_error_fn key_extractor) in
  let reverse = Compare_behavior.reverse compare_behavior in
  let handle_missing = Compare_behavior.handle_missing compare_behavior in
  combine_extract_transform_and_compare_into_sort_key_module
    ~key_extractor
    ~extract
    ~transform
    ~compare:Float.compare
    ~handle_missing
    ~reverse
;;

module Chain_sort (A : Sort_key) (B : Sort_key) : Sort_key = struct
  type key = A.key * B.key

  let extract sexp : key Extraction_result.t =
    match A.extract sexp, B.extract sexp with
    (* We always surface errors, even if the behavior for one key says we should drop the
       input sexp. *)
    | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Drop, _ -> Drop
    | _, Drop -> Drop
    | Ok a, Ok b -> Ok (a, b)
  ;;

  let compare (a1, b1) (a2, b2) =
    let a_compare = A.compare a1 a2 in
    if a_compare = 0 then B.compare b1 b2 else a_compare
  ;;
end

let with_default_compare_behavior = function
  | None -> None
  | Some extractors ->
    Some (List.map extractors ~f:(fun e -> e, Compare_behavior.default))
;;

let field_flag =
  let%map_open.Command fields =
    Key_extractor.field_param ~doc:"FIELD Sort by the value associated with this field" ()
  in
  with_default_compare_behavior fields
;;

let index_flag =
  let%map_open.Command indexes =
    Key_extractor.index_param
      ~doc:"INDEX Sort by the value at this index in the top-level of a sexp"
      ()
  in
  with_default_compare_behavior indexes
;;

let query_flag =
  let%map_open.Command fields =
    Key_extractor.query_param
      ~doc:"QUERY Sort by the values referenced by this query, as used in sexp query"
      ()
  in
  with_default_compare_behavior fields
;;

let pat_query_flag =
  let%map_open.Command fields =
    Key_extractor.pat_query_param
      ~doc:"PATTERN Sort by the values reference by this query, as used in sexp pat-query"
      ()
  in
  with_default_compare_behavior fields
;;

let get_flag =
  let%map_open.Command fields =
    Key_extractor.get_param
      ~doc:"PATH Sort by the values referenced by this path, as used in sexp get"
      ()
  in
  with_default_compare_behavior fields
;;

let select_flag =
  let%map_open.Command fields =
    Key_extractor.select_param
      ~doc:"PATH Sort by the values referenced by this path, as used in sexp select"
      ()
  in
  with_default_compare_behavior fields
;;

let key_flag =
  Key_extractor.general_param
    ~doc:"KEY Sort by the key"
    ~modifiers:
      (Map
         (fun modifiers ~flag_and_arg ->
           match modifiers with
           | None -> Compare_behavior.default
           | Some modifiers -> Compare_behavior.from_modifiers modifiers flag_and_arg))
    ()
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
       | Ok (Some y) -> foldi_until_err (i + 1) pipe (y :: accum)
       | Ok None -> foldi_until_err (i + 1) pipe accum)
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
Sorts a list of s-expressions. The sort key can be specified in a variety of formats:
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
      asc    -> sort in ascending order (default)
      desc   -> sort in descending (reverse) order
      rev    -> sort in reverse order
      nat    -> use natural sort
      num    -> use numeric sort
      str    -> use string sort (default)
      s      -> use case-sensitive sort (default)
      i      -> use case-insensitive sort
      merror -> raise an error if the key is missing (default)
      mfirst -> output sexps with missing keys first
      mlast  -> output sexps with missing keys last
      mdrop  -> drop sexps with missing keys from the output
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
           (Default_to [ Key_extractor.identity_extractor, Compare_behavior.default ])
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
     and handle_missing = How_to_handle_missing.flag in
     fun () ->
       let top_level_compare_behavior =
         Compare_behavior.from_top_level_flags
           ~numeric
           ~natural
           ~case_insensitive
           ~reverse
           ~handle_missing
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
       let extract_or_drop_or_error i sexp =
         match Sorter.extract sexp with
         | Ok key -> Ok (Some { key; sexp })
         | Drop -> Ok None
         | Error s ->
           Or_error.error_string ("Error on sexp " ^ Int.to_string (i + 1) ^ ": " ^ s)
       in
       let sexps_in = Reader.read_sexps (Lazy.force Reader.stdin) in
       match%bind pipe_mapi_result sexps_in ~f:extract_or_drop_or_error with
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

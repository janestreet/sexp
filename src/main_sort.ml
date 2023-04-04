open Core
open Async

(* Helper for error messages. *)
let quoted flag = "\"" ^ flag ^ "\""

(* Command does some automatic handling of exceptions and prints out errors
   as sexps. When combined with the quoting behavior above, this leads to lots
   of ugly escaping. We provide our own custom error function that just prints
   out the message and immediately exits to avoid this. *)
let cmd_error msg =
  Core.prerr_endline msg;
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

  let from_modifiers modifiers flag_and_arg =
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
          ("Unknown sort behavior modifier " ^ quoted unknown ^ " in " ^ flag_and_arg));
    (* These checks are nearly the same as the ones above in in from_top_level_flags,
       but we print out slightly different error messages, so duplicate the logic here. *)
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
  let extract = unstage (Key_extractor.extract_or_error_fn key_extractor ~transform) in
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
  let extract = unstage (Key_extractor.extract_or_error_fn key_extractor ~transform) in
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

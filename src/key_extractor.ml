open Core
open Sexp_app
module Sexp_path = Sexplib.Path

type t =
  | Identity
  | Query of
      { query : Syntax.Query.t
      ; flag_and_arg : string
      }
  | Pat_query of
      { pat_query : Sexp_app_pattern.Query.t
      ; flag_and_arg : string
      }
  | Select of
      { program : string
      ; flag_and_arg : string
      }

(* Command does some automatic handling of exceptions and prints out errors as sexps. When
   combined with the quoting behavior above, this leads to lots of ugly escaping. We
   provide our own custom error function that just prints out the message and immediately
   exits to avoid this. *)
let cmd_error msg =
  Core.prerr_endline msg;
  Core.exit 1
;;

let cmd_error_s msg sexp =
  Core.prerr_endline (msg ^ "\n  " ^ Sexp.to_string_hum ~indent:2 sexp);
  Core.exit 1
;;

(* Helper functions for cleaner error messages. *)
let quoted flag = "\"" ^ flag ^ "\""

(*=We really just need to pass in the arg that we got from the command line
   to create an extractor, but when an error occurs, we want to tell the user
   which flag caused an error, in case they specified multiple keys. For example:

   > "Key specified by '-field foo' appears multiple times"

   Usually the flag and arg can be inferred from the type of extractor we're
   building; e.g., a field extractor was specified by writing "-field <field>".

   But when specifying keys using multiple extraction types, as in sexp sort,
   we use the special syntax "-key field:foo", which we also want to echo
   back to the user as "-key field:foo", instead of "-field foo", which isn't
   what they wrote.

   So the <access_kind>_extractor functions take in optional arguments for the
   actual flag name and argument that were passed in, then we combine those into
   a single string that we use for error messages.

   The flag values often include whitespace, so they need to be quoted.
   [Sys.quote] will add single quotes if a string has any special characters,
   but then to separate the provided flag from the rest of the error message,
   we will wrap it in double quotes. We won't escape any double quotes inside
   the flag value because 1) this won't happen that often, 2) the error message
   isn't getting interpreted so it's fine to not be escaped, and 3) because it
   makes it harder to read.
*)

let quoted_flag_and_arg ~flag ~arg = "\"-" ^ flag ^ " " ^ Sys.quote arg ^ "\""
let identity_extractor = Identity

let none_if_empty = function
  | [] -> None
  | l -> Some l
;;

(* index extraction; do this one first because it takes an int, so the types don't work
   out quite as nicely *)

let index_extractor ~flag ~arg index =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
  Query { query = Index index; flag_and_arg }
;;

let index_param ?(flag = "index") ~doc () =
  let flag_name = flag in
  let%map_open.Command indexes = flag flag_name (listed int) ~doc in
  List.map indexes ~f:(fun index ->
    index_extractor ~flag:flag_name ~arg:(Int.to_string index) index)
  |> none_if_empty
;;

(* All of the <access_kind>_extractor functions (other than "index" above) have the same
   type. As a result, building a [t list option Command.Param.t] on top of an extraction
   function is the same for every access kind, with just the name of the flag and the
   <access_kind>_extraction function changing. *)
let build_param_function
  default_flag_name
  (extractor_constructor : flag:string -> arg:string -> string -> t)
  =
  let fn ?(flag = default_flag_name) ~doc () =
    let flag_name = flag in
    let%map_open.Command args = flag flag_name (listed string) ~doc in
    List.map args ~f:(fun arg -> extractor_constructor ~flag:flag_name ~arg arg)
    |> none_if_empty
  in
  fn
;;

(* field extraction *)

let field_extractor ~flag ~arg field =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
  Query { query = Field field; flag_and_arg }
;;

let field_param = build_param_function "field" field_extractor

(* query extraction *)

let query_extractor ~flag ~arg query =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
  (* A little clunky vs. just using parse_string_exn, but we prioritize providing a little
     more context to the error message, which may be helpful if using sexp sort in a long
     pipeline. *)
  let query_sexps =
    match Parsexp.Many.parse_string query with
    | Ok x -> x
    | Error e ->
      cmd_error_s
        ("Invalid sexp query [QUERY] passed as " ^ flag_and_arg)
        [%sexp (e : Parsexp.Parse_error.t)]
  in
  Query
    { query = Syntax.pipe (List.map ~f:Syntax.Query.t_of_sexp query_sexps); flag_and_arg }
;;

let query_param = build_param_function "query" query_extractor

(* pat-query extraction *)

let pat_query_extractor ~flag ~arg pat_query =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
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
       ^ flag_and_arg);
  if num_number_captures = 0 && num_unlabeled_captures = 0
  then cmd_error ("pat-query [PATTERN] must specify at least one capture: " ^ flag_and_arg);
  Pat_query { pat_query; flag_and_arg }
;;

let pat_query_param = build_param_function "pat-query" pat_query_extractor

(* get extraction *)

(* a little compiler from the sort of path expressions you would pass to [sexp get] to the
   sort of query you would pass to [sexp query] *)
let sexp_query_expression_of_get_query_string path flag_and_arg =
  let path_parts =
    try Sexp_path.parse path with
    | exn ->
      cmd_error_s ("Bad sexp get [PATH] passed as " ^ flag_and_arg) [%sexp (exn : Exn.t)]
  in
  let query_sexps =
    List.concat_map path_parts ~f:(function
      | Pos n -> [ Syntax.Index n ]
      | Match (tag, n) -> Syntax.[ Test (Variant (tag, None)); Index (n + 1) ]
      | Rec name -> [ Syntax.Field name ])
  in
  Syntax.pipe query_sexps
;;

let get_extractor ~flag ~arg path =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
  Query
    { query = sexp_query_expression_of_get_query_string path flag_and_arg; flag_and_arg }
;;

let get_param = build_param_function "get" get_extractor

(* select extraction *)

let select_extractor ~flag ~arg program =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg in
  Select { program; flag_and_arg }
;;

let select_param = build_param_function "select" select_extractor

(* general key extraction *)

let modifier_sep = '/'
let modifier_sep_str = "/"

let extract_modifiers specifier =
  match String.split specifier ~on:modifier_sep with
  | [] -> failwith "impossible"
  | [ specifier ] -> specifier, None
  | specifier :: modifiers -> specifier, Some modifiers
;;

(* Returns an extractor and optionally a list of modifiers if any were specified. *)
let general_extractor ~flag full_arg =
  let flag_and_arg = quoted_flag_and_arg ~flag ~arg:full_arg in
  match String.lsplit2 ~on:':' full_arg with
  | None -> cmd_error ("Invalid -key format (no ':' separator): " ^ flag_and_arg)
  | Some (specifier, key_arg) ->
    let specifier, modifiers = extract_modifiers specifier in
    let extractor =
      match specifier with
      | "field" -> field_extractor ~flag ~arg:full_arg key_arg
      | "index" ->
        let index = Int.of_string key_arg in
        index_extractor ~flag ~arg:full_arg index
      | "query" -> query_extractor ~flag ~arg:full_arg key_arg
      | "pat-query" -> pat_query_extractor ~flag ~arg:full_arg key_arg
      | "get" -> get_extractor ~flag ~arg:full_arg key_arg
      | "select" -> select_extractor ~flag ~arg:full_arg key_arg
      | specifier ->
        cmd_error ("Unknown key specifier " ^ quoted specifier ^ " in " ^ flag_and_arg)
    in
    extractor, modifiers
;;

type _ modifiers_handler =
  | Not_supported : t modifiers_handler
  | Map : (string list option -> flag_and_arg:string -> 'a) -> (t * 'a) modifiers_handler

let general_param
  (type a)
  ?(flag = "key")
  ~doc
  ~modifiers:(how_to_handle_modifiers : a modifiers_handler)
  ()
  : a list option Command.Param.t
  =
  let flag_name = flag in
  let%map_open.Command keys = flag flag_name (listed string) ~doc in
  List.map keys ~f:(fun key : a ->
    let flag_and_arg = quoted_flag_and_arg ~flag:flag_name ~arg:key in
    let extractor, modifiers = general_extractor ~flag:flag_name key in
    match how_to_handle_modifiers, modifiers with
    | Not_supported, None -> extractor
    | Not_supported, Some modifiers ->
      let modifiers = String.concat ~sep:modifier_sep_str modifiers in
      cmd_error ("Cannot pass modifiers " ^ quoted modifiers ^ " in " ^ flag_and_arg)
    | Map f, _ -> extractor, f modifiers ~flag_and_arg)
  |> none_if_empty
;;

(* actual logic for building the extract function *)

let extract_fn = function
  | Identity -> fun sexp -> [ sexp ]
  | Query { query; _ } -> fun sexp -> Lazy_list.to_list (Semantics.query query sexp)
  | Pat_query { pat_query; _ } -> unstage (Pat_query.run pat_query)
  | Select { program; _ } -> unstage (Sexp_select.select_staged program)
;;

let flag_and_arg = function
  | Identity -> None
  | Query { flag_and_arg; _ } | Pat_query { flag_and_arg; _ } | Select { flag_and_arg; _ }
    -> Some flag_and_arg
;;

module Extraction_error = struct
  (* This is a slightly awkward type. I considered getting rid of the [Missing_key]
     constructor, and having the extract function return a [Sexp.t option] instead of just
     a [Sexp.t], but this results in an extra allocation in the common case when you
     actually have a key. It also means that there's only one kind of error,
     [Multiple_keys], which would be slightly weird.

     Another alternative would be to define a custom [Result.t] and have the extract
     function return that, instead of an [(Sexp.t, Extraction_error.t) result], with three
     variants:

     {[
       module Result = struct
         type t =
           | Key of Sexp.t
           | Missing_key of string
           | Multiple_keys of string
       end
     ]}

     The appeal here is that this interface doesn't make a judgment about whether a
     missing key is an error or not. sexp sort and sexp group both have flags to control
     the behavior around how missing keys are handled. But neither had that functionality
     to start; they both errored on missing keys in their initial versions. Forcing the
     caller to handle that case forces them to think too far down the road. It's fine to
     implement the happy path of assuming the key always exists when writing a tool, then
     later on figure out how to evolve it.

     This [Result.t] type also has other issues: it's unclear that the strings in the
     non-[Key] constructors are supposed to be error messages. Also, [Multiple_keys]
     should always be an error, but we're forcing people to match on a specific type of
     error. Making the third constructor just be [Error of Error.t] and then having
     [Error.t] just contain [Multiple_keys] seems like overkill.

     I did actually implement this, and, ultimately, using the standard [result] type is
     just semantically clearer, and callers can match on the nested errors if they wish.
  *)
  type t =
    | Missing_key
    | Multiple_keys
end

let extractor_source t =
  match flag_and_arg t with
  | None -> ""
  | Some flag_and_arg -> " specified by " ^ flag_and_arg
;;

(* Returns a projection function that extracts a key from a sexp and applies a
   transformation function. Will error if no key is extracted from the sexp, or if
   multiple keys are extracted. Will also bubble up an error returned by the
   transformation function. *)
let extract_or_error_fn t =
  let extract = extract_fn t in
  let extractor_source = extractor_source t in
  let missing_key_error_str = "Missing key" ^ extractor_source in
  let multiple_keys_error_str = "Key" ^ extractor_source ^ " occurs multiple times" in
  let project sexp =
    match extract sexp with
    | [] -> Error (Extraction_error.Missing_key, missing_key_error_str)
    | [ key ] -> Ok key
    | _ -> Error (Extraction_error.Multiple_keys, multiple_keys_error_str)
  in
  stage project
;;

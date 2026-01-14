open! Core

(* This module exposes a [Key_extractor.t] type that can be used to extract values from a
   [Sexp.t] using one of a predefined set of common access patterns. This is used for
   extracting sort keys for sexp sort.

   The access patterns match the capabilities of the various filtering commands:
   - sexp query
   - sexp pat-query
   - sexp get
   - sexp select

   That is, given a string you could pass to one of the above commands, you can build a
   [Key_extractor.t] that can then be used to create a function that will return the part
   of a [Sexp.t] that the filter command would extract.

   (The field and index extractors are special cases of the common sexp query commands
   "(field <name>)" and "(index <N>)".)

   Some of the filter patterns can extract multiple values (e.g., a sexp select query
   could return fields with the same name nested at different levels of the sexp). The
   expectation is that each input [Sexp.t] has a single key, so an error will be returned
   if the extraction results in multiple values (or no values!).

   Key extractors are designed to be specified from the command line, so we also take in a
   flag name that can used to provide errors with a little more context.

   When building the extract function, we also provide a transformation function to
   perform additional transformations on just the extracted part of the key.
*)

type t

(* An extractor that returns the entire input [Sexp.t]. *)
val identity_extractor : t

(* Build extractors from command line arguments that are the equivalent to running sexp
   query "(field <field>)" on the input [Sexp.t]. *)
val field_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

(* Build extractors from command line arguments that are the equivalent to running sexp
   query "(index <N>)" on the input [Sexp.t]. *)
val index_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

(* Build extractors from command line arguments that are the equivalent to running sexp
   query <query> on the input [Sexp.t]. *)
val query_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

(* Build extractors from command line arguments that are the equivalent to running sexp
   pat-query <pat_query> on the input [Sexp.t]. *)
val pat_query_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

(* Build extractors from command line arguments that are the equivalent to running sexp
   get <path> on the input [Sexp.t]. *)
val get_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

(* Build extractors from command line arguments that are the equivalent to running sexp
   select <program> on the input [Sexp.t]. *)
val select_param : ?flag:string -> doc:string -> unit -> t list option Command.Param.t

type _ modifiers_handler =
  | Not_supported : t modifiers_handler
  | Map : (string list option -> flag_and_arg:string -> 'a) -> (t * 'a) modifiers_handler

(*=Build extractors from command line arguments of the form "<access_kind><modifiers?>:<arg>"
   where:
   - <access_kind> is one of (field|index|query|pat-query|get|select)
   - <modifiers> are optional strings whose meaning is defined by the caller.
     The <access_kind> and each modifier should be separated by a '/'.
   - <arg> is the arg you would pass to <access_kind>_extractor.

   If you don't want to support modifiers, pass in [modifiers:Not_supported], and
   you will get back a list of extractors.

   If you do want to support modifiers, pass in [modifiers:(Map some_fn)], where
   some_fn is passed a [string list option] of modifiers. You will get back a list
   of extractors paired with the return value of some_fn.

   For example, building a general extractor from "query/mod_a/mod_b:(field foo)" will
   be equivalent to building a query extractor from the string "(field foo)" and it
   will return ["mod_a"; "mod_b"] as the modifiers. *)
val general_param
  :  ?flag:string
  -> doc:string
  -> modifiers:'a modifiers_handler
  -> unit
  -> 'a list option Command.Param.t

module Extraction_error : sig
  type t =
    | Missing_key
    | Multiple_keys
end

(* Build a function that will extract out part of the input [Sexp.t] that is specified by
   the key extractor. It will error if nothing is extracted from the input [Sexp.t], or if
   multiple values are extracted. The [string] paired with the [Extraction_error.t] in the
   [Error] case can be used in an error message to indicate which flag on the command line
   was used when getting this value. *)
val extract_or_error_fn
  :  t
  -> (Sexp.t -> (Sexp.t, Extraction_error.t * string) result) Staged.t

(* A helper for producing nicer error message; returns a string like ' specified by -field
   foo' to clarify which key lookup failed if there are multiple keys specified. *)
val extractor_source : t -> string

(* A helper for producing nicer error messages; combines flag name and a command line
   argument into a double quoted string. *)
val quoted_flag_and_arg : flag:string -> arg:string -> string

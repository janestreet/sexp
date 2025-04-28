open! Core

module Format : sig
  (** A format specifies how the user wants their captured results to be formatted by
      providing a sexp structure in which the captures will get embedded. *)
  type t =
    | Atom of string
    | Capture of string
    | List of t list
  [@@deriving sexp]

  val ts_of_string : string -> t list
  val all_captures : t -> string list
  val embed_captures : t -> f:(string -> Sexp.t list) -> Sexp.t list
end

module Wrap_mode : sig
  (** {v
 This controls what happens when a single capture expression consumes multiple sexps
      during a single match. It specifies whether to wrap them together as a single sexp
      list or return all the results separately.

      For example:
      "(a %[.*])" tries to unwrap exactly one set of parens, match an 'a', and then
      capture all the sexps that follow that 'a'. Here are the three behaviors.

      Wrap_always:
      (a) -> Sexp.List []                                    (* () *)
      (a b) -> Sexp.List [ Sexp.Atom b ]                     (* (b) *)
      (a b c) -> Sexp.List [ Sexp.Atom b; Sexp.Atom c ]      (* (b c) *)

      Wrap_non_singletons:
      (a) -> Sexp.List []                                    (* () *)
      (a b) -> Sexp.Atom b                                   (* b *)
      (a b c) -> Sexp.List [ Sexp.Atom b; Sexp.Atom c ]      (* (b c) *)

      Unwrap_always:
      (a) -> []                                              (*  *)
      (a b) -> [ Sexp.Atom b ]                               (* b *)
      (a b c) -> [ Sexp.Atom b; Sexp.Atom c ]                (* b c *)

      This wrapping (or not) occurs before packing the result into whatever format
      specified by [Output_method.t] below.

      Since [Unwrap_always] has the possiblity of returning multiple sexps separately,
      the [`capture] type for it is a list of sexps instead of a single sexp.
      v} *)
  type 'capture t =
    | Wrap_always : Sexp.t t
    | Wrap_non_singletons : Sexp.t t
    | Unwrap_always : Sexp.t list t
  [@@deriving sexp_of]

  type some_wrap_mode = T : _ t -> some_wrap_mode
end

type _ t =
  | Formats : _ Wrap_mode.t * Format.t list -> Sexp.t list t
  (** Embed captures in the specified formats *)
  | List : _ Wrap_mode.t -> Sexp.t t
  (** Return different capture expressions' results as a Sexp.List. In the case of
      [Unwrap_always], the sequences consumed by each capture expression are concatenated,
      so the list may be longer (or shorter) than the number of capture expressions. *)
  | Record : _ Wrap_mode.t -> Sexp.t t
  (** Return captures as a sexp record where the field names are the labels of the
      capturing expressions. In the case of [Unwrap_always], the sequences consumed by
      each capture expression have the field name consed onto them, so the result may not
      actually be a list of pairs! *)
  | Single_capture : 'query_result Wrap_mode.t -> 'query_result t
  (** Expect exactly one capture in the pattern, and return its captured contents. *)
  | Map : Sexp.t list String.Map.t t
  (** Return a map from capture name to captures. Similar to doing [Record Wrap_always]
      and then [[%of_sexp: Sexp.t list String.Map.t]] *)
[@@deriving sexp_of]

type some_output_method = T : _ t -> some_output_method

(** Determine a default output method to use based on whether the query contains numbered
    or named captures. *)
val default_method : Query.t -> wrap_mode:_ Wrap_mode.t -> some_output_method

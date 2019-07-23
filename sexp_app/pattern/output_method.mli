open! Core

module Format : sig
  (** A format specifies how the user wants their captured results to be formatted
      by providing a sexp structure in which the captures will get embedded. *)
  type t =
    | Atom of string
    | Capture of string
    | List of t list
  [@@deriving sexp]

  val ts_of_string : string -> t list
  val all_captures : t -> string list
  val embed_captures : t -> f:(string -> Sexp.t) -> Sexp.t
end

module Desired : sig
  type _ t =
    | Formats : Format.t list -> Sexp.t list t
    (** Embed captures in the specified formats *)
    | Default : Sexp.t t (** Choose a default automatically *)
    | Map : Sexp.t list String.Map.t t (** Return a map from capture name to captures *)
  [@@deriving sexp_of]
end

module Compiled : sig
  (** Same as [Desired.t], but [Default] has been resolved into a specific method *)
  type _ t = private
    | Formats : Format.t list -> Sexp.t list t
    (** Embed captures in the specified formats *)
    | List : Sexp.t t (** Return captures as a sexp list *)
    | Record : Sexp.t t (** Return captures as a sexp record *)
    | Single_capture : Sexp.t t (** Expect exactly one capture, and return it alone *)
    | Map : Sexp.t list String.Map.t t (** Return a map from capture name to captures *)
  [@@deriving sexp_of]

  (** Determine which actual output method to use based on the desired method and whether
      the captures are named/numbered. *)
  val of_desired
    :  'output_type Desired.t
    -> has_named_captures:bool
    -> has_number_captures:bool
    -> num_unlabeled_captures:int
    -> 'output_type t
end

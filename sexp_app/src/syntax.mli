open! Core

type regex = Re2.t [@@deriving sexp]

module Template : sig
  type 'a t =
    | Hole of 'a
    | Atom of string
    | List of 'a t list

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val literal : Sexp.t -> 'a t
end

module Quotation : sig
  (*=
     a non-regular datatype for quasi-quotation
     due to Ross Paterson and Richard Bird
     "de Bruijn notation as a nested datatype"
     JFP 9(1): 77-91. 1999
  *)

  type 'a t =
    | Atom of string
    | List of 'a t list
    | Quote of 'a t t
    | Unquote of 'a
    | Splice of 'a
  [@@deriving sexp]
end

module Var : sig
  include Identifiable.S

  val is_list : t -> bool
end

module type Pattern_general = sig
  type t = Var.t Template.t [@@deriving sexp]

  val pmatch : t -> Sexp.t -> fail:(unit -> 'a) -> succ:(Sexp.t Var.Table.t -> 'a) -> 'a
  val instantiate : t -> Sexp.t Var.Table.t -> (Sexp.t -> 'b) -> 'b
end

module Pattern : Pattern_general
module Pattern_record : Pattern_general

type 'a anti_quote =
  | Unquote of 'a
  | Splice of 'a

val flatten : 'a Quotation.t -> 'a anti_quote Template.t

module Record_field : sig
  type 'change t =
    { change : 'change
    ; new_name : string option
    ; presence : [ `Present | `Optional | `Absent ]
    }
end

type query =
  | This
  | Pipe of query * query
  | Die
  | Cat of query * query
  | Equals of Sexp.t Hash_set.t
  | Regex of regex
  | Variant of string * int option
  | Field of string
  | Index of int
  | Each
  | Smash
  | Atomic
  | Length
  | Wrap of query
  | Test of query
  | Not of query
  | And of query * query
  | Or of query * query
  | If of query * query * query
  | Branch of query * query * query
  | Quote of query anti_quote Template.t
  | Change of change
  | Restructure

and change =
  | Id
  | Fail
  | Delete
  | Alt of change * change
  | Seq of change * change
  | Children of change
  | Record of change Record_field.t String.Map.t
  | Rewrite of Pattern.t * Pattern.t
  | Rewrite_record of Pattern_record.t * Pattern_record.t
  | Topdown of change
  | Bottomup of change
  | Lowercase
  | Concat
  | Query of query

val pipe : query list -> query
val cat : query list -> query
val or_ : query list -> query
val and_ : query list -> query
val quote : query Quotation.t -> query
val equals : Sexp.t -> query
val try_ : change -> change
val alt : change list -> change
val seq : change list -> change
val const : Sexp.t -> change

(* recursive syntactic sugar *)
module Unroll : sig
  val topdown : change -> change
  val bottomup : change -> change
end

module Query : sig
  type t = query [@@deriving sexp]
end

module Change : sig
  type t = change [@@deriving sexp]
end

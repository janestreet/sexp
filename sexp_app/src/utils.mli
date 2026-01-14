open! Core

val simple_query : Syntax.Query.t -> Sexp.t -> Sexp.t list

(** [ get_fields sexp field ] searches for field recursively *)
val get_fields : Sexp.t -> string -> Sexp.t list

(** [ get_one_field sexp field ] searches for field recursively and returns the value, or
    error if there is not exactly one result. *)
val get_one_field : Sexp.t -> string -> Sexp.t Or_error.t

(** [ get_immediate_fields sexp ] returns an association list of field names to field
    values, or error if the sexp does not conform to the normal structure of an ocaml
    record. *)
val immediate_fields : Sexp.t -> (string, Sexp.t) List.Assoc.t Or_error.t

(** [ to_record_sexp by_field ] converts an association list of field names to field
    values into a record-like sexp. *)
val to_record_sexp : (string, Sexp.t) List.Assoc.t -> Sexp.t

(** [ sexp_rewrite sexp ~f ] returns the rewritten sexp where f is applied to sexp and its
    descendents. *)
val sexp_rewrite : Sexp.t -> f:(Sexp.t -> [ `Unchanged | `Changed of Sexp.t ]) -> Sexp.t

(** [ replace_field ~field ~value sexp scope ] replaces either the field with the given
    value, or returns an error if no match is found in the designated scope. If `Recursive
    is chosen, it will replace as many instances as appear, or error if none are found. *)
val replace_field
  :  field:string
  -> value:Sexp.t
  -> Sexp.t
  -> [ `Immediate | `Recursive ]
  -> Sexp.t Or_error.t
  @@ portable

(** [ remove_field ~field sexp scope ] removes the field if it exists in the designated
    scope. If `Recursive is chosen, it will replace as many instances as appear. It will
    not error if the field is not in scope *)
val remove_field
  :  field:string
  -> Sexp.t
  -> [ `Immediate | `Recursive ]
  -> Sexp.t Or_error.t

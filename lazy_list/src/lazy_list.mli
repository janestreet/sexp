open! Core

(** Lazy lists. *)

type 'a t

include Monad.S with type 'a t := 'a t

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val decons : 'a t -> ('a * 'a t) option
val cons : 'a -> 'a t -> 'a t
val snoc : 'a t -> 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val concat : 'a t t -> 'a t
val nth : 'a t -> int -> 'a option
val concat_list : 'a list t -> 'a t
val find : f:('a -> bool) -> 'a t -> 'a option
val filter : f:('a -> bool) -> 'a t -> 'a t
val filter_opt : 'a option t -> 'a t
val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b

(* [foldr ~f t ~init] is a lazy version of [fold_right] that doesn't necessarily force a
   traversal of the entire list, as is more natural for a lazy list.
*)

val foldr : 'a t -> f:('a -> 'b Lazy.t -> 'b) -> init:'b -> 'b Lazy.t
val iter : 'a t -> f:('a -> unit) -> unit
val of_iterator : curr:('a -> 'b option) -> next:('a -> 'a) -> init:'a -> 'b t
val build : f:('s -> ('a * 's) option) -> seed:'s -> 'a t
val unfold : f:('a -> 'a option) -> init:'a -> 'a t
val uniter : f:(unit -> 'a option) -> 'a t
val of_list : 'a list -> 'a t
val to_rev_list : 'a t -> 'a list
val to_list : 'a t -> 'a list
val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val cartesian_product : 'a t t -> 'a t t
val merge : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

val unify
  :  cmp:('a -> 'b -> int)
  -> 'a t
  -> 'b t
  -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] t

val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val lazy_sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t

(* constructing lazy lists from values of container types *)
module Of_container : sig
  module type T = sig
    type 'a t

    val lazy_fold : 'a t -> f:('a -> 'b Lazy.t -> 'b) -> last:'b -> 'b
  end

  (* (applications of) this module are meant to be included into *)
  module Make (X : T) : sig
    val lazy_list_of_t : 'a X.t -> 'a t
  end
end

(* Iterators are useful when you're trying to avoid closing over the head of a lazy list
   to avoid a space leak. Just create one of these outside said closure and close over the
   iterator instead.
*)
module Iterator : sig
  type 'a lazy_list = 'a t
  type 'a t

  val create : 'a lazy_list -> 'a t

  (* Produces the next element in the list and updates the iterator *)

  val next : 'a t -> 'a option
  val iter : 'a t -> f:('a -> unit) -> unit
end

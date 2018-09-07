(** mutable lists with addition to the end *)

type 'a t

val create : unit -> 'a t
val add : 'a t -> 'a -> unit
val to_list : 'a t -> 'a list

open! Core

(* container type where each element is associated with some labeled input source *)

type 'a t

(* [stdin label_opt] indicates standard input as a labeled sources *)

val stdin : string option -> unit t

(* [files [f1; f2; ... ;fn]] indicates files [f1] ... [f2] as sources labeled with their
   own filenames
*)

val files : string list -> unit t

(* open all associated input channels *)

val channels : unit t -> In_channel.t t

(* map across the container *)

val map : 'a t -> f:('a -> 'b) -> 'b t

(* iterate across elements together with their associated source labels *)

val iter : 'a t -> f:(string -> 'a -> unit) -> unit

open Core

type 'a t = 'a list ref

let create () = ref []
let add t x = t := x :: !t
let to_list t = List.rev !t

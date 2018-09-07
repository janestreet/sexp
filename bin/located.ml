open Core
module X = Lazy_list

let of_list (xs : 'a list) : 'a X.t = X.of_list xs

type 'a t =
  | Stdin of string option * 'a
  | Files of (string * 'a) X.t

let map t ~f =
  match t with
  | Stdin (label, x) -> Stdin (label, f x)
  | Files fs -> Files (X.map fs ~f:(fun (label, x) -> label, f x))
;;

let iter t ~f =
  match t with
  | Stdin (label, x) ->
    let label = Option.value ~default:"stdin" label in
    f label x
  | Files fs -> X.iter fs ~f:(fun (label, x) -> f label x)
;;

let stdin label_opt = Stdin (label_opt, ())
let files fs = Files (X.map (of_list fs) ~f:(fun f -> f, ()))

let channels = function
  | Stdin (label, ()) -> Stdin (label, In_channel.stdin)
  | Files fs ->
    let f (fname, ()) = fname, In_channel.create fname in
    Files (X.map fs ~f)
;;

open Core

(* Creating a pad and adding a bunch of strings to it has asymtotic time and space
   complexity O(n) where n is the total number of characters. Every once and a while,
   there will be a very expensive [add] operation.
*)

(** INVARIANT: strings are in order of decreasing length from right to left *)
type t =
  | Empty
  | Snoc of t * string

let empty = Empty

let rec add t x =
  match t with
  | Empty -> Snoc (Empty, x)
  | Snoc (rest, y) ->
    if String.length y > String.length x then Snoc (t, x) else add rest (y ^ x)
;;

let singleton x = add empty x
let add_char t c = add t (String.of_char c)

let rec dump = function
  | Empty -> ""
  | Snoc (Empty, x) -> x
  | Snoc (Snoc (t, x), y) -> dump (Snoc (t, x ^ y))
;;

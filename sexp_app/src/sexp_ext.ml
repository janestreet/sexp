open! Core

type t =
  | Atom of string
  | List of t Lazy_list.t

let rec t_of_sexp = function
  | Sexp.Atom x -> Atom x
  | Sexp.List xs -> List (Lazy_list.map ~f:t_of_sexp (Lazy_list.of_list xs))
;;

let rec cps_map xs ~f k =
  match xs with
  | [] -> k []
  | x :: xs -> f x (fun y -> cps_map xs ~f (fun ys -> k (y :: ys)))
;;

let rec sexp_of_t t k =
  match t with
  | Atom x -> k (Sexp.Atom x)
  | List xs -> cps_map ~f:sexp_of_t (Lazy_list.to_list xs) (fun xs -> k (Sexp.List xs))
;;

let sexp_of_t t = sexp_of_t t (fun x -> x)

let equal =
  let rec equal_loop = function
    | [] -> true
    | xy :: pending ->
      (match xy with
       | Sexp.Atom x, Atom y -> String.equal x y && equal_loop pending
       | Sexp.List xs, List ys -> combine_loop xs ys pending
       | _ -> false)
  and combine_loop xs ys pending =
    match xs, Lazy_list.decons ys with
    | [], None -> equal_loop pending
    | x :: xs, Some (y, ys) -> combine_loop xs ys ((x, y) :: pending)
    | _ -> false
  in
  fun x y -> equal_loop [ x, y ]
;;

let rec lowercase = function
  | Atom x -> Atom (String.lowercase x)
  | List xs -> List (Lazy_list.map ~f:lowercase xs)
;;

let rec sub_expressions x =
  Lazy_list.cons
    x
    (match x with
     | Atom _ -> Lazy_list.empty ()
     | List xs -> Lazy_list.bind xs ~f:sub_expressions)
;;

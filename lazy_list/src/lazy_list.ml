open Core

type 'a node =
  | Empty
  | Cons of 'a * 'a lazy_list

and 'a lazy_list = 'a node Lazy.t

let rec map t ~f =
  Lazy.map t ~f:(function
    | Empty -> Empty
    | Cons (x, xs) -> Cons (f x, map xs ~f))
;;

module Base : sig
  type 'a t = 'a lazy_list

  val empty : unit -> 'a t
  val return : 'a -> 'a t
  val map : [> `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end = struct
  type 'a t = 'a lazy_list

  let empty () = Lazy.from_val Empty
  let return x = Lazy.from_val (Cons (x, Lazy.from_val Empty))

  let rec append t1 t2 =
    Lazy.map t1 ~f:(function
      | Empty -> Lazy.force t2
      | Cons (x, xs) -> Cons (x, append xs t2))
  ;;

  let rec concat t =
    Lazy.map t ~f:(function
      | Empty -> Empty
      | Cons (x, xs) -> Lazy.force (append x (concat xs)))
  ;;

  let bind m ~f = concat (map ~f m)
  let map = `Custom map
end

type 'a t = 'a Base.t

include (Monad.Make (Base) : Monad.S with type 'a t := 'a t)

let empty = Base.empty
let append = Base.append
let concat = Base.concat

let is_empty t =
  match Lazy.force t with
  | Cons _ -> false
  | Empty -> true
;;

let length t =
  let rec loop n t =
    match Lazy.force t with
    | Cons (_, t) -> loop (n + 1) t
    | Empty -> n
  in
  loop 0 t
;;

let decons t =
  match Lazy.force t with
  | Empty -> None
  | Cons (h, t) -> Some (h, t)
;;

let cons x t = Lazy.from_val (Cons (x, t))

let rec snoc t x =
  Lazy.map t ~f:(function
    | Empty -> Cons (x, Base.empty ())
    | Cons (y, ys) -> Cons (y, snoc ys x))
;;

let rec find ~f t =
  match Lazy.force t with
  | Empty -> None
  | Cons (x, xs) -> if f x then Some x else find ~f xs
;;

let rec filter ~f t =
  Lazy.bind t ~f:(function
    | Empty -> empty ()
    | Cons (x, xs) -> if f x then cons x (filter ~f xs) else filter ~f xs)
;;

let rec filter_opt t =
  Lazy.bind t ~f:(function
    | Empty -> empty ()
    | Cons (Some x, xs) -> cons x (filter_opt xs)
    | Cons (None, xs) -> filter_opt xs)
;;

let rec filter_map ~f t =
  Lazy.bind t ~f:(function
    | Empty -> empty ()
    | Cons (x, xs) ->
      (match f x with
       | Some y -> cons y (filter_map ~f xs)
       | None -> filter_map ~f xs))
;;

let rec fold_left ~f ~init t =
  match Lazy.force t with
  | Empty -> init
  | Cons (x, xs) -> fold_left ~f xs ~init:(f init x)
;;

let to_rev_list t = fold_left t ~init:[] ~f:(fun xs x -> x :: xs)
let to_list t = List.rev (to_rev_list t)
let fold_right ~f t ~init = List.fold (to_rev_list t) ~init ~f:(fun a b -> f b a)

let rec foldr t ~f ~init =
  Lazy.map t ~f:(function
    | Empty -> init
    | Cons (x, xs) -> f x (foldr ~f xs ~init))
;;

let rec iter t ~f =
  match Lazy.force t with
  | Empty -> ()
  | Cons (x, xs) ->
    f x;
    iter ~f xs
;;

let of_iterator ~curr ~next ~init =
  let rec loop accum () =
    match curr accum with
    | Some x -> Cons (x, Lazy.from_fun (loop (next accum)))
    | None -> Empty
  in
  Lazy.from_fun (loop init)
;;

let rec build ~f ~seed =
  Lazy.from_fun (fun () ->
    match f seed with
    | None -> Empty
    | Some (x, seed) -> Cons (x, build ~f ~seed))
;;

module Of_container = struct
  module type T = sig
    type 'a t

    val lazy_fold : 'a t -> f:('a -> 'b Lazy.t -> 'b) -> last:'b -> 'b
  end

  module Make (X : T) = struct
    let lazy_list_of_t x =
      Lazy.from_fun (fun () ->
        X.lazy_fold x ~f:(fun x seed -> Cons (x, seed)) ~last:Empty)
    ;;
  end
end

let unfold ~f ~init =
  let rec loop accum () =
    match f accum with
    | Some x -> Cons (x, Lazy.from_fun (loop x))
    (*=| Some(x) -> Cons(accum, Lazy.from_fun (loop x)) *)
    | None -> Empty
  in
  Lazy.from_fun (loop init)
;;

let uniter ~f =
  let rec loop () =
    match f () with
    | Some x -> Cons (x, Lazy.from_fun loop)
    | None -> Empty
  in
  Lazy.from_fun loop
;;

let rec of_list xs =
  Lazy.from_fun (fun () ->
    match xs with
    | [] -> Empty
    | x :: xs -> Cons (x, of_list xs))
;;

let concat_list t = concat (map t ~f:of_list)

let of_array ary =
  let rec loop i () =
    if i < Array.length ary then Cons (ary.(i), Lazy.from_fun (loop (succ i))) else Empty
  in
  Lazy.from_fun (loop 0)
;;

let rec nth xs i =
  if i < 0
  then None
  else (
    match Lazy.force xs with
    | Empty -> None
    | Cons (x, xs) -> if i = 0 then Some x else nth xs (i - 1))
;;

let to_array t =
  match Lazy.force t with
  | Empty -> [||]
  | Cons (x, xs) ->
    let ary = Array.create ~len:(length t) x in
    let i = ref 1 in
    iter xs ~f:(fun x ->
      ary.(!i) <- x;
      incr i);
    ary
;;

let rec merge ~cmp xlst ylst =
  Lazy.bind xlst ~f:(function
    | Empty -> ylst
    | Cons (x, xs) ->
      Lazy.bind ylst ~f:(function
        | Empty -> xlst
        | Cons (y, ys) ->
          if cmp x y <= 0
          then cons x (merge ~cmp xs ylst)
          else cons y (merge ~cmp xlst ys)))
;;

let rec unify ~cmp xlst ylst =
  Lazy.bind xlst ~f:(function
    | Empty -> map ylst ~f:(fun y -> `Right y)
    | Cons (x, xs) ->
      Lazy.bind ylst ~f:(function
        | Empty -> map xlst ~f:(fun x -> `Left x)
        | Cons (y, ys) ->
          (match cmp x y with
           | -1 -> cons (`Left x) (unify ~cmp xs ylst)
           | 0 -> cons (`Both (x, y)) (unify ~cmp xs ys)
           | 1 -> cons (`Right y) (unify ~cmp xlst ys)
           | _ -> assert false)))
;;

let lazy_sort ~cmp zlst =
  (* This is a stable, O(N log N) worst-case, merge sort.  It has the
   * additional useful property that forcing the first element only takes
   * O(N) time, and forcing each additional element only takes O(log N)
   * time, meaning it is worthwhile to sort a lazy list even if you only
   * want the first few elements.
   *
   * The basic strategy is as follows: we convert the lazy list into a
   * (normal) list of one element long lazy lists.  We then go through
   * merging pairs of lazy lists together, into 2 element long lazy lists,
   * then 4 element long lazy lists, etc., until we merge all the lists
   * back into one big list (that is now in sorted order).
   *
   * In building the final list, we end up creating about 2N intermediate
   * lists (2N-1, I think).  Forcing the first element forces the first
   * element of all of these lists, meaning that it is O(N) cost to do so.
   * But we only remove the heads of O(log N) of these lists (those lists
   * whose head element is the head element of the sorted list)- so forcing
   * the second element only takes O(log N) work.  And so on for the third
   * element, etc.
  *)
  let rec to_zlist_list accum = function
    | Empty -> accum
    | Cons (x, xs) -> to_zlist_list (return x :: accum) (Lazy.force xs)
  in
  let rec merge_pairs reversed accum = function
    | x1 :: x2 :: xs ->
      if reversed
      then merge_pairs reversed (merge ~cmp x2 x1 :: accum) xs
      else merge_pairs reversed (merge ~cmp x1 x2 :: accum) xs
    | [ x ] -> x :: accum
    | [] -> accum
  in
  let rec merge_all_pairs reversed = function
    | [] -> empty ()
    | [ x ] -> x
    | lst -> merge_all_pairs (not reversed) (merge_pairs reversed [] lst)
  in
  merge_all_pairs true (to_zlist_list [] (Lazy.force zlst))
;;

let sort ~cmp zlst =
  (* We inline to_array here, so we can control where we catch the
   * invalid_argument exception Array.create ~len:throws when we try to
   * make too large of an array.
  *)
  match Lazy.force zlst with
  | Empty -> zlst
  | Cons (x, xs) ->
    (* Note, a little convolution is necessary here, as I want to trap
     * Invalid_argument exceptions *only* around the Array.create ~len:call.
     * Remember that iterating through the lazy list is potientially
     * executing code that could potientially throw Invalid_argument
     * for entirely other reasons, and I don't want to catch those
     * exceptions.
    *)
    let ary_opt =
      try Some (Array.create ~len:(length zlst) x) with
      | Invalid_argument _ -> None
    in
    (match ary_opt with
     | None ->
       (* Array was too large- abort to lazy_sort *)
       lazy_sort ~cmp zlst
     | Some ary ->
       (* Fill the array *)
       let i = ref 1 in
       iter xs ~f:(fun x ->
         ary.(!i) <- x;
         incr i);
       (* Sort the array *)
       Array.sort ~compare:cmp ary;
       (* Return the lazy list of the array *)
       of_array ary)
;;

module Iterator = struct
  type 'a lazy_list = 'a t
  type 'a t = 'a lazy_list ref

  let create zlst = ref zlst

  let next t =
    match decons !t with
    | Some (hd, tl) ->
      t := tl;
      Some hd
    | None -> None
  ;;

  let iter t ~f =
    let rec loop () =
      match next t with
      | Some item ->
        f item;
        loop ()
      | None -> ()
    in
    loop ()
  ;;
end

(* cartesian_product : [[a]] -> [[a]] *)
let rec cartesian_product t =
  match Lazy.force t with
  | Empty -> return (empty ())
  | Cons (xs, xss) ->
    xs >>= fun y -> cartesian_product xss >>= fun ys -> return (cons y ys)
;;

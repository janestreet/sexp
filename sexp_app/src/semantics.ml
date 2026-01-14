open Core
open Lazy_list.Let_syntax

include struct
  open Syntax
  module Change = Change
  module Pattern = Pattern
  module Pattern_record = Pattern_record
  module Query = Query
  module Template = Template
  module Unroll = Unroll
end

type 'sexp query_meaning = 'sexp -> Sexp.t Lazy_list.t

(* This module contains multiple implementations for the same interface [S]. Each
   implementation improves on the prior one in some way, but at the cost of readability.
   To help with maintenance of the code, the entire sequence of implementations is
   preserved here. We ultimately end up using the final implementation in the sequence.
*)
module type S = sig
  val query : Query.t -> Sexp.t query_meaning
  val change : Change.t -> Sexp.t -> Sexp.t option
end

type sexp = Sexp.t =
  | Atom of string
  | List of sexp list

(* cheap import *)

let rec sexp_collapse = function
  | Atom s -> s
  | List ss -> String.concat (List.map ss ~f:sexp_collapse)
;;

let sexp_to_record = function
  | Atom _ -> None
  | List fields ->
    with_return (fun r ->
      match
        String.Map.of_alist
          (List.mapi fields ~f:(fun i -> function
             | List [ Atom field; s ] -> field, (i, s)
             | _ -> r.return None))
      with
      | `Duplicate_key _ -> None
      | `Ok m -> Some m)
;;

let restructure acc s ~nil ~of_list =
  match (s : Sexp.t) with
  | List _ -> nil acc
  | Atom s ->
    (match Sexp.scan_sexps (Lexing.from_string s) with
     | exception _ -> nil acc
     | sexps -> of_list sexps acc)
;;

(* the reference implementation *)
module _ : S = struct
  let nil : Sexp.t Lazy_list.t = Lazy_list.empty ()
  let cons = Lazy_list.cons
  let one s = Lazy_list.cons s nil
  let ( >>= ) = Lazy_list.( >>= )
  let ( ++ ) = Lazy_list.append

  let atomic = function
    | Atom _ as s -> one s
    | List _ -> nil
  ;;

  let int i = one ([%sexp_of: int] i)

  let length = function
    | Atom _ -> int 1
    | List l -> int (List.length l)
  ;;

  let index i = function
    | Atom _ -> nil
    | List xs ->
      (match List.nth xs i with
       | None -> nil
       | Some s -> one s)
  ;;

  type 'a changed =
    | Fail
    | Same
    | Delete
    | Diff of 'a

  type block =
    | One of Sexp.t
    | Row of Sexp.t list
    | Col of Sexp.t Lazy_list.t

  let rec smash s =
    match s with
    | Atom _ -> one s
    | List ss -> cons s (Lazy_list.of_list ss >>= smash)
  ;;

  let each s =
    match s with
    | Atom _ -> nil
    | List ss -> Lazy_list.of_list ss
  ;;

  let field name s =
    match s with
    | Atom _ -> nil
    | List ss ->
      (match%bind Lazy_list.of_list ss with
       | List [ Atom fname; fvalue ] when String.( = ) fname name -> one fvalue
       | _ -> nil)
  ;;

  let variant name n_opt s =
    match s with
    | Atom cname when String.( = ) cname name ->
      (match n_opt with
       | None -> one s
       | Some n -> if n = 0 then one s else nil)
    | List (Atom cname :: args) when String.( = ) cname name ->
      (match n_opt with
       | None -> one s
       | Some n -> if n = List.length args then one s else nil)
    | _ -> nil
  ;;

  let rec atom_map ~f = function
    | Atom s -> Atom (f s)
    | List xs -> List (List.map xs ~f:(fun s -> atom_map ~f s))
  ;;

  let regex r = function
    | Atom s ->
      if Re2.num_submatches r > 1
      then (
        try one (Atom (Re2.find_first_exn ~sub:(`Index 1) r s)) with
        | _ -> nil)
      else if Re2.matches r s
      then one (Atom s)
      else nil
    | List _ -> nil
  ;;

  let rec query q s =
    match q with
    | Syntax.This -> one s
    | Syntax.Die -> nil
    | Syntax.Length -> length s
    | Syntax.Atomic -> atomic s
    | Syntax.Index i -> index i s
    | Syntax.Smash -> smash s
    | Syntax.Pipe (q1, q2) ->
      let%bind s = query q1 s in
      query q2 s
    | Syntax.Cat (q1, q2) -> query q1 s ++ query q2 s
    | Syntax.Each -> each s
    | Syntax.Field f -> field f s
    | Syntax.Variant (c, n) -> variant c n s
    | Syntax.Equals s' -> if Hash_set.mem s' s then one s else nil
    | Syntax.Regex r -> regex r s
    | Syntax.If (q1, q2, q3) ->
      (match Lazy_list.decons (query q1 s) with
       | Some _ -> query q2 s
       | None -> query q3 s)
    | Syntax.Branch (q1, q2, q3) ->
      (match Lazy_list.decons (query q1 s) with
       | Some (x, xs) ->
         let%bind s = cons x xs in
         query q2 s
       | None -> query q3 s)
    | Syntax.And (q1, q2) ->
      (match Lazy_list.decons (query q1 s) with
       | Some _ -> query q2 s
       | None -> nil)
    | Syntax.Or (q1, q2) ->
      (match Lazy_list.decons (query q1 s) with
       | Some (x, xs) -> cons x xs
       | None -> query q2 s)
    | Syntax.Wrap q -> one (List (Lazy_list.to_list (query q s)))
    | Syntax.Test q ->
      (match Lazy_list.decons (query q s) with
       | Some _ -> one s
       | None -> nil)
    | Syntax.Not q ->
      (match Lazy_list.decons (query q s) with
       | Some _ -> nil
       | None -> one s)
    | Syntax.Quote t ->
      (match quote t s with
       | One x -> one x
       | Row xs -> Lazy_list.of_list xs
       | Col xs -> xs)
    | Syntax.Change c ->
      (match change c s with
       | Same -> one s
       | Diff s -> one s
       | Fail | Delete -> nil)
    | Syntax.Restructure ->
      restructure
        ()
        s
        ~nil:(fun () -> nil)
        ~of_list:(fun sexps () -> Lazy_list.of_list sexps)

  and quote t s =
    match t with
    | Template.Atom a -> One (Atom a)
    | Template.Hole (Syntax.Unquote q) -> Col (query q s)
    | Template.Hole (Syntax.Splice q) -> Row (Lazy_list.to_list (query q s))
    | Template.List ts ->
      let rec rows
        : Syntax.Query.t Syntax.anti_quote Template.t list -> Sexp.t list Lazy_list.t
        = function
        | [] -> cons [] (Lazy_list.empty ())
        | t :: ts ->
          let blocks = rows ts in
          (match quote t s with
           | One x -> Lazy_list.map blocks ~f:(fun ys -> x :: ys)
           | Row xs -> Lazy_list.map blocks ~f:(fun ys -> xs @ ys)
           | Col xs ->
             let%bind x = xs in
             Lazy_list.map blocks ~f:(fun ys -> x :: ys))
      in
      Col
        (let%bind row = rows ts in
         one (List row))

  and change c s =
    match c with
    | Syntax.Id -> Same
    | Syntax.Fail -> Fail
    | Syntax.Delete -> Delete
    | Syntax.Seq (c1, c2) ->
      (match change c1 s with
       | Fail -> Fail
       | Same -> change c2 s
       | Delete -> Delete
       | Diff s ->
         (match change c2 s with
          | Delete -> Delete
          | Fail -> Fail
          | Same -> Diff s
          | Diff s -> Diff s))
    | Syntax.Alt (c1, c2) ->
      (match change c1 s with
       | Same -> Same
       | Delete -> Delete
       | Diff s -> Diff s
       | Fail -> change c2 s)
    | Syntax.Topdown c -> change (Unroll.topdown c) s
    | Syntax.Bottomup c -> change (Unroll.bottomup c) s
    | Syntax.Children c ->
      (match s with
       | Atom _ -> Same
       | List ss ->
         (match children c ss with
          | Delete -> assert false (* [children] doesn't return [delete] *)
          | Fail -> Fail
          | Same -> Same
          | Diff ss -> Diff (List ss)))
    | Syntax.Record _ ->
      (* Only implemented in [Nofun]. *)
      assert false
    | Syntax.Rewrite (lhs, rhs) ->
      Pattern.pmatch
        lhs
        s
        ~fail:(fun () -> Fail)
        ~succ:(fun env -> Diff (Pattern.instantiate rhs env Fn.id))
    | Syntax.Rewrite_record (lhs, rhs) ->
      Pattern_record.pmatch
        lhs
        s
        ~fail:(fun () -> Fail)
        ~succ:(fun env -> Diff (Pattern_record.instantiate rhs env Fn.id))
    | Syntax.Lowercase -> Diff (atom_map ~f:String.lowercase s)
    | Syntax.Concat ->
      (match s with
       | Atom _ -> Same
       | List _ -> Diff (Atom (sexp_collapse s)))
    | Syntax.Query q ->
      Diff
        (List
           (List.rev
              (Lazy_list.fold_left (query q s) ~init:[] ~f:(fun acc x -> x :: acc))))

  and children c = function
    | [] -> Same
    | s :: ss ->
      (match change c s with
       | Fail -> Fail
       | Delete -> children c ss
       | Same ->
         (match children c ss with
          | Fail -> Fail
          | Delete -> Diff [ s ]
          | Same -> Same
          | Diff ss -> Diff (s :: ss))
       | Diff s ->
         (match children c ss with
          | Fail -> Fail
          | Delete -> Diff [ s ]
          | Same -> Diff (s :: ss)
          | Diff ss -> Diff (s :: ss)))
  ;;

  let change c s =
    match change c s with
    | Delete -> None
    | Fail -> None
    | Same -> Some s
    | Diff s -> Some s
  ;;
end

(* conversion of [Vanilla] to continuation-passing style *)
module _ : S = struct
  module Internal : sig
    open Syntax

    type ('a, 'r) seq = ('a, 'r) cont -> 'r
    and ('a, 'r) cont = ('a, 'r) node -> 'r

    and ('a, 'r) node =
      | Nil
      | Cons of 'a * ('a, 'r) seq

    val query : Query.t -> Sexp.t -> (Sexp.t, 'r) seq

    val change
      :  Change.t
      -> Sexp.t
      -> fail:(unit -> 'r)
      -> same:(unit -> 'r)
      -> diff:(Sexp.t -> 'r)
      -> delete:(unit -> 'r)
      -> 'r
  end = struct
    type ('a, 'r) seq = ('a, 'r) cont -> 'r
    and ('a, 'r) cont = ('a, 'r) node -> 'r

    and ('a, 'r) node =
      | Nil
      | Cons of 'a * ('a, 'r) seq

    let nil k = k Nil
    let one s k = k (Cons (s, nil))

    let rec app m n k =
      m (function
        | Nil -> n k
        | Cons (x, xs) -> k (Cons (x, fun k -> app xs n k)))
    ;;

    let rec bind m ~f k =
      m (function
        | Nil -> k Nil
        | Cons (x, xs) -> app (fun k -> f x k) (fun k -> bind xs ~f k) k)
    ;;

    let rec map m ~f k =
      m (function
        | Nil -> k Nil
        | Cons (x, xs) -> f x (fun y -> k (Cons (y, fun k -> map xs ~f k))))
    ;;

    let atomic s k =
      match s with
      | Atom _ as s -> one s k
      | List _ -> nil k
    ;;

    let length s k =
      match s with
      | Atom _ -> one ([%sexp_of: int] 1) k
      | List l -> one ([%sexp_of: int] (List.length l)) k
    ;;

    let index i s k =
      match s with
      | Atom _ -> nil k
      | List xs ->
        (match List.nth xs i with
         | None -> nil k
         | Some s -> one s k)
    ;;

    let rec of_list xs k =
      match xs with
      | [] -> nil k
      | y :: ys -> k (Cons (y, fun k -> of_list ys k))
    ;;

    (*=let rec to_list xs k =
      xs (function
      | Nil -> k []
      | Cons (y, ys) -> to_list ys (fun ys -> k (y :: ys)))
      ;;*)
    let rec smash s k =
      match s with
      | Atom _ -> one s k
      | List ss -> k (Cons (s, fun k -> bind (fun k -> of_list ss k) ~f:smash k))
    ;;

    let each s k =
      match s with
      | Atom _ -> nil k
      | List ss -> of_list ss k
    ;;

    let field name s k =
      match s with
      | Atom _ -> nil k
      | List ss ->
        bind
          (fun k -> of_list ss k)
          ~f:(fun s k ->
            match s with
            | List [ Atom fname; fvalue ] when String.( = ) fname name -> one fvalue k
            | _ -> nil k)
          k
    ;;

    let variant name n_opt s k =
      match s with
      | Atom cname when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = 0 then one s k else nil k)
      | List (Atom cname :: args) when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = List.length args then one s k else nil k)
      | _ -> nil k
    ;;

    let rec list_map xs ~f k =
      match xs with
      | [] -> k []
      | x :: xs -> f x (fun x -> list_map xs ~f (fun xs -> k (x :: xs)))
    ;;

    let rec atom_map ~f s k =
      match s with
      | Atom x -> k (Atom (f x))
      | List ss -> list_map ss ~f:(fun s k -> atom_map ~f s k) (fun ss -> k (List ss))
    ;;

    type ('a, 'r) cache =
      | Evaluated of 'a
      | Unevaluated of (('a -> 'r) -> 'r)

    let memo m =
      let cache = ref (Unevaluated m) in
      fun k ->
        match !cache with
        | Evaluated v -> k v
        | Unevaluated m ->
          m (fun v ->
            cache := Evaluated v;
            k v)
    ;;

    let regex r s k =
      match s with
      | Atom s ->
        if Re2.num_submatches r > 1
        then (
          try one (Atom (Re2.find_first_exn ~sub:(`Index 1) r s)) k with
          | _ -> nil k)
        else if Re2.matches r s
        then one (Atom s) k
        else nil k
      | List _ -> nil k
    ;;

    let rec query q s k =
      match q with
      | Syntax.This -> one s k
      | Syntax.Die -> nil k
      | Syntax.Atomic -> atomic s k
      | Syntax.Length -> length s k
      | Syntax.Index i -> index i s k
      | Syntax.Smash -> smash s k
      | Syntax.Pipe (q1, q2) ->
        bind (fun k -> query q1 s k) ~f:(fun s k -> query q2 s k) k
      | Syntax.Cat (q1, q2) -> app (fun k -> query q1 s k) (fun k -> query q2 s k) k
      | Syntax.Each -> each s k
      | Syntax.Field f -> field f s k
      | Syntax.Variant (c, n) -> variant c n s k
      | Syntax.Equals s' -> if Hash_set.mem s' s then one s k else nil k
      | Syntax.Regex r -> regex r s k
      | Syntax.If (q1, q2, q3) ->
        query q1 s (function
          | Cons (_, _) -> query q2 s k
          | Nil -> query q3 s k)
      | Syntax.Branch (q1, q2, q3) ->
        query q1 s (function
          | Cons (x, xs) ->
            bind (fun k -> k (Cons (x, xs))) ~f:(fun s k -> query q2 s k) k
          | Nil -> query q3 s k)
      | Syntax.And (q1, q2) ->
        query q1 s (function
          | Cons (_, _) -> query q2 s k
          | Nil -> nil k)
      | Syntax.Or (q1, q2) ->
        query q1 s (function
          | Cons (x, xs) -> k (Cons (x, xs))
          | Nil -> query q2 s k)
      | Syntax.Wrap q -> to_list (fun k -> query q s k) (fun xs -> one (List xs) k)
      | Syntax.Test q ->
        query q s (function
          | Cons (_, _) -> one s k
          | Nil -> nil k)
      | Syntax.Not q ->
        query q s (function
          | Cons (_, _) -> nil k
          | Nil -> one s k)
      | Syntax.Quote t ->
        quote
          t
          s
          ~dot:(fun x -> one x k)
          ~row:(fun xs -> of_list xs k)
          ~col:(fun xs -> xs k)
      | Syntax.Change c ->
        change
          c
          s
          ~fail:(fun () -> nil k)
          ~delete:(fun () -> nil k)
          ~same:(fun () -> one s k)
          ~diff:(fun s -> one s k)
      | Syntax.Restructure -> restructure k s ~nil ~of_list

    and quote t s ~dot ~col ~row =
      match t with
      | Template.Atom a -> dot (Atom a)
      | Template.Hole (Syntax.Unquote q) -> col (fun k -> query q s k)
      | Template.Hole (Syntax.Splice q) -> to_list (fun k -> query q s k) row
      | Template.List ts ->
        let rec rows
          :  Syntax.Query.t Syntax.anti_quote Template.t list
          -> ((Sexp.t list, 'r) node -> 'r) -> 'r
          =
          fun ts k ->
          match ts with
          | [] -> k (Cons ([], nil))
          | t :: ts ->
            let blocks k = rows ts k in
            quote
              t
              s
              ~dot:(fun x -> map blocks ~f:(fun ys k -> k (x :: ys)) k)
              ~row:(fun xs -> map blocks ~f:(fun ys k -> k (xs @ ys)) k)
              ~col:(fun xs ->
                let blocks = memo blocks in
                bind xs ~f:(fun x k -> map blocks ~f:(fun ys k -> k (x :: ys)) k) k)
        in
        col (fun k -> bind (fun k -> rows ts k) ~f:(fun row k -> one (List row) k) k)

    and change c s ~fail ~same ~diff ~delete =
      match c with
      | Syntax.Id -> same ()
      | Syntax.Fail -> fail ()
      | Syntax.Delete -> delete ()
      | Syntax.Seq (c1, c2) ->
        change
          c1
          s
          ~fail
          ~same:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~diff:(fun s -> change c2 s ~fail ~same:(fun () -> diff s) ~diff ~delete)
          ~delete
      | Syntax.Alt (c1, c2) ->
        change
          c1
          s
          ~same
          ~diff
          ~fail:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~delete
      | Syntax.Topdown c -> change (Unroll.topdown c) s ~fail ~same ~diff ~delete
      | Syntax.Bottomup c -> change (Unroll.bottomup c) s ~fail ~same ~diff ~delete
      | Syntax.Children c ->
        (match s with
         | Atom _ -> same ()
         | List ss -> children c ss ~fail ~same ~diff:(fun ss -> diff (List ss)))
      | Syntax.Record _ ->
        (* Only implemented in [Nofun]. *)
        assert false
      | Syntax.Rewrite (lhs, rhs) ->
        Pattern.pmatch lhs s ~fail ~succ:(fun env -> Pattern.instantiate rhs env diff)
      | Syntax.Rewrite_record (lhs, rhs) ->
        Pattern_record.pmatch lhs s ~fail ~succ:(fun env ->
          Pattern_record.instantiate rhs env diff)
      | Syntax.Lowercase -> atom_map s ~f:String.lowercase diff
      | Syntax.Concat ->
        (match s with
         | Atom _ -> same ()
         | List _ -> diff (Atom (sexp_collapse s)))
      | Syntax.Query q ->
        query q s (function
          | Nil -> diff (List []) (* or [fail ()] *)
          | Cons (x, v) -> to_list v (fun xs -> diff (List (x :: xs))))

    and children c ss ~fail ~same ~diff =
      match ss with
      | [] -> same ()
      | s :: ss ->
        change
          c
          s
          ~fail
          ~same:(fun () -> children c ss ~fail ~same ~diff:(fun ss -> diff (s :: ss)))
          ~diff:(fun s ->
            children
              c
              ss
              ~fail
              ~same:(fun () -> diff (s :: ss))
              ~diff:(fun ss -> diff (s :: ss)))
          ~delete:(fun () -> children c ss ~fail ~same ~diff)

    and to_list v k =
      v (function
        | Nil -> k []
        | Cons (x, v) -> to_list v (fun xs -> k (x :: xs)))
    ;;
  end

  type state = State of (Sexp.t, state) Internal.node

  let query q s =
    Lazy_list.build ~seed:(Internal.query q s) ~f:(fun sexps ->
      match sexps (fun x -> State x) with
      | State Internal.Nil -> None
      | State (Internal.Cons (x, v)) -> Some (x, v))
  ;;

  let change c s =
    Internal.change
      c
      s
      ~fail:(fun () -> None)
      ~delete:(fun () -> None)
      ~same:(fun () -> Some s)
      ~diff:(fun s -> Some s)
  ;;
end

(* monomorphization of [Cont] b/c it makes the next translation easier *)
module _ : S = struct
  module Internal : sig
    open Syntax

    type 'r seq = 'r cont -> 'r
    and 'r cont = 'r node -> 'r

    and 'r node =
      | Nil
      | Cons of Sexp.t * 'r seq

    val query : Query.t -> Sexp.t -> 'r seq

    val change
      :  Change.t
      -> Sexp.t
      -> fail:(unit -> 'r)
      -> same:(unit -> 'r)
      -> diff:(Sexp.t -> 'r)
      -> delete:(unit -> 'r)
      -> 'r
  end = struct
    type 'r seq = 'r cont -> 'r
    and 'r cont = 'r node -> 'r

    and 'r node =
      | Nil
      | Cons of Sexp.t * 'r seq

    type 'r seq' = 'r cont' -> 'r
    and 'r cont' = 'r node' -> 'r

    and 'r node' =
      | Nil'
      | Cons' of Sexp.t list * 'r seq'

    let nil k = k Nil
    let nil' k = k Nil'
    let one s k = k (Cons (s, nil))

    let rec app m n k =
      m (function
        | Nil -> n k
        | Cons (x, xs) -> k (Cons (x, fun k -> app xs n k)))
    ;;

    let rec app' m n k =
      m (function
        | Nil' -> n k
        | Cons' (x, xs) -> k (Cons' (x, fun k -> app' xs n k)))
    ;;

    let rec bind m ~f k =
      m (function
        | Nil -> k Nil
        | Cons (x, xs) -> app (fun k -> f x k) (fun k -> bind xs ~f k) k)
    ;;

    let rec bind_' m f k =
      m (function
        | Nil -> k Nil'
        | Cons (x, xs) -> app' (fun k -> f x k) (fun k -> bind_' xs f k) k)
    ;;

    let rec bind'_ m f k =
      m (function
        | Nil' -> k Nil
        | Cons' (x, xs) -> app (fun k -> f x k) (fun k -> bind'_ xs f k) k)
    ;;

    let rec map' m ~f k =
      m (function
        | Nil' -> k Nil'
        | Cons' (x, xs) -> f x (fun y -> k (Cons' (y, fun k -> map' xs ~f k))))
    ;;

    let atomic s k =
      match s with
      | Atom _ as s -> one s k
      | List _ -> nil k
    ;;

    let length s k =
      match s with
      | Atom _ -> one ([%sexp_of: int] 1) k
      | List l -> one ([%sexp_of: int] (List.length l)) k
    ;;

    let index i s k =
      match s with
      | Atom _ -> nil k
      | List xs ->
        (match List.nth xs i with
         | None -> nil k
         | Some s -> one s k)
    ;;

    let rec of_list xs k =
      match xs with
      | [] -> nil k
      | y :: ys -> k (Cons (y, fun k -> of_list ys k))
    ;;

    let rec to_list xs k =
      xs (function
        | Nil -> k []
        | Cons (y, ys) -> to_list ys (fun ys -> k (y :: ys)))
    ;;

    let rec smash s k =
      match s with
      | Atom _ -> one s k
      | List ss -> k (Cons (s, fun k -> bind (fun k -> of_list ss k) ~f:smash k))
    ;;

    let each s k =
      match s with
      | Atom _ -> nil k
      | List ss -> of_list ss k
    ;;

    let field name s k =
      match s with
      | Atom _ -> nil k
      | List ss ->
        bind
          (fun k -> of_list ss k)
          ~f:(fun s k ->
            match s with
            | List [ Atom fname; fvalue ] when String.( = ) fname name -> one fvalue k
            | _ -> nil k)
          k
    ;;

    let variant name n_opt s k =
      match s with
      | Atom cname when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = 0 then one s k else nil k)
      | List (Atom cname :: args) when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = List.length args then one s k else nil k)
      | _ -> nil k
    ;;

    let rec list_map xs ~f k =
      match xs with
      | [] -> k []
      | x :: xs -> f x (fun x -> list_map xs ~f (fun xs -> k (x :: xs)))
    ;;

    let rec atom_map ~f s k =
      match s with
      | Atom x -> k (Atom (f x))
      | List ss -> list_map ss ~f:(fun s k -> atom_map ~f s k) (fun ss -> k (List ss))
    ;;

    type ('a, 'r) cache =
      | Evaluated of 'a
      | Unevaluated of (('a -> 'r) -> 'r)

    let memo m =
      let cache = ref (Unevaluated m) in
      fun k ->
        match !cache with
        | Evaluated v -> k v
        | Unevaluated m ->
          m (fun v ->
            cache := Evaluated v;
            k v)
    ;;

    let regex r s k =
      match s with
      | Atom s ->
        if Re2.num_submatches r > 1
        then (
          try one (Atom (Re2.find_first_exn ~sub:(`Index 1) r s)) k with
          | _ -> nil k)
        else if Re2.matches r s
        then one (Atom s) k
        else nil k
      | List _ -> nil k
    ;;

    let rec query q s k =
      match q with
      | Syntax.This -> one s k
      | Syntax.Die -> nil k
      | Syntax.Atomic -> atomic s k
      | Syntax.Length -> length s k
      | Syntax.Index i -> index i s k
      | Syntax.Smash -> smash s k
      | Syntax.Pipe (q1, q2) ->
        bind (fun k -> query q1 s k) ~f:(fun s k -> query q2 s k) k
      | Syntax.Cat (q1, q2) -> app (fun k -> query q1 s k) (fun k -> query q2 s k) k
      | Syntax.Each -> each s k
      | Syntax.Field f -> field f s k
      | Syntax.Variant (c, n) -> variant c n s k
      | Syntax.Equals s' -> if Hash_set.mem s' s then one s k else nil k
      | Syntax.Regex r -> regex r s k
      | Syntax.If (q1, q2, q3) ->
        query q1 s (function
          | Cons (_, _) -> query q2 s k
          | Nil -> query q3 s k)
      | Syntax.Branch (q1, q2, q3) ->
        query q1 s (function
          | Cons (x, xs) ->
            bind (fun k -> k (Cons (x, xs))) ~f:(fun s k -> query q2 s k) k
          | Nil -> query q3 s k)
      | Syntax.And (q1, q2) ->
        query q1 s (function
          | Cons (_, _) -> query q2 s k
          | Nil -> nil k)
      | Syntax.Or (q1, q2) ->
        query q1 s (function
          | Cons (x, xs) -> k (Cons (x, xs))
          | Nil -> query q2 s k)
      | Syntax.Wrap q -> to_list (fun k -> query q s k) (fun xs -> one (List xs) k)
      | Syntax.Test q ->
        query q s (function
          | Cons (_, _) -> one s k
          | Nil -> nil k)
      | Syntax.Not q ->
        query q s (function
          | Cons (_, _) -> nil k
          | Nil -> one s k)
      | Syntax.Quote t ->
        quote
          t
          s
          ~dot:(fun x -> one x k)
          ~row:(fun xs -> of_list xs k)
          ~col:(fun xs -> xs k)
      | Syntax.Change c ->
        change
          c
          s
          ~fail:(fun () -> nil k)
          ~delete:(fun () -> nil k)
          ~same:(fun () -> one s k)
          ~diff:(fun s -> one s k)
      | Syntax.Restructure -> restructure k s ~nil ~of_list

    and quote t s ~dot ~(col : ('r cont -> 'r) -> 'r) ~row =
      match t with
      | Template.Atom a -> dot (Atom a)
      | Template.Hole (Syntax.Unquote q) -> col (fun k -> query q s k)
      | Template.Hole (Syntax.Splice q) -> to_list (fun k -> query q s k) row
      | Template.List ts ->
        let rec rows
          (ts : Syntax.Query.t Syntax.anti_quote Template.t list)
          (k : 'r cont')
          : 'r
          =
          match ts with
          | [] -> k (Cons' ([], nil'))
          | t :: ts ->
            let blocks k = rows ts k in
            quote
              t
              s
              ~dot:(fun x -> map' blocks ~f:(fun ys k -> k (x :: ys)) k)
              ~row:(fun xs -> map' blocks ~f:(fun ys k -> k (xs @ ys)) k)
              ~col:(fun xs ->
                let blocks = memo blocks in
                bind_' xs (fun x k -> map' blocks ~f:(fun ys k -> k (x :: ys)) k) k)
        in
        col (fun k -> bind'_ (fun k -> rows ts k) (fun row k -> one (List row) k) k)

    and change c s ~fail ~same ~diff ~delete =
      match c with
      | Syntax.Id -> same ()
      | Syntax.Fail -> fail ()
      | Syntax.Delete -> delete ()
      | Syntax.Seq (c1, c2) ->
        change
          c1
          s
          ~fail
          ~same:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~diff:(fun s -> change c2 s ~fail ~same:(fun () -> diff s) ~diff ~delete)
          ~delete
      | Syntax.Alt (c1, c2) ->
        change
          c1
          s
          ~same
          ~diff
          ~fail:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~delete
      | Syntax.Topdown c -> change (Unroll.topdown c) s ~fail ~same ~diff ~delete
      | Syntax.Bottomup c -> change (Unroll.bottomup c) s ~fail ~same ~diff ~delete
      | Syntax.Children c ->
        (match s with
         | Atom _ -> same ()
         | List ss -> children c ss ~fail ~same ~diff:(fun ss -> diff (List ss)))
      | Syntax.Record _ ->
        (* Only implemented in [Nofun]. *)
        assert false
      | Syntax.Rewrite (lhs, rhs) ->
        Pattern.pmatch lhs s ~fail ~succ:(fun env -> Pattern.instantiate rhs env diff)
      | Syntax.Rewrite_record (lhs, rhs) ->
        Pattern_record.pmatch lhs s ~fail ~succ:(fun env ->
          Pattern_record.instantiate rhs env diff)
      | Syntax.Lowercase -> atom_map s ~f:String.lowercase diff
      | Syntax.Concat ->
        (match s with
         | Atom _ -> same ()
         | List _ -> diff (Atom (sexp_collapse s)))
      | Syntax.Query q ->
        query q s (function
          | Nil -> diff (List []) (* or [fail ()] *)
          | Cons (x, v) -> to_list v (fun xs -> diff (List (x :: xs))))

    and children c ss ~fail ~same ~diff =
      match ss with
      | [] -> same ()
      | s :: ss ->
        change
          c
          s
          ~fail
          ~same:(fun () -> children c ss ~fail ~same ~diff:(fun ss -> diff (s :: ss)))
          ~diff:(fun s ->
            children
              c
              ss
              ~fail
              ~same:(fun () -> diff (s :: ss))
              ~diff:(fun ss -> diff (s :: ss)))
          ~delete:(fun () -> children c ss ~fail ~same ~diff)
    ;;
  end

  type state = State of state Internal.node

  let query q s =
    Lazy_list.build ~seed:(Internal.query q s) ~f:(fun sexps ->
      match sexps (fun x -> State x) with
      | State Internal.Nil -> None
      | State (Internal.Cons (x, v)) -> Some (x, v))
  ;;

  let change c s =
    Internal.change
      c
      s
      ~fail:(fun () -> None)
      ~delete:(fun () -> None)
      ~same:(fun () -> Some s)
      ~diff:(fun s -> Some s)
  ;;
end

(* defunctionalization of [Mono] to eliminate the overhead of constructing so many
   closures. *)
module Nofun : S = struct
  module Internal : sig
    open Syntax

    type 'r seq

    type 'r node =
      | Nil
      | Cons of Sexp.t * 'r seq

    val pull : 'r seq -> ('r node -> 'r) -> 'r
    val query : Query.t -> Sexp.t -> 'r seq

    val change
      :  Change.t
      -> Sexp.t
      -> fail:(unit -> 'r)
      -> same:(unit -> 'r)
      -> diff:(Sexp.t -> 'r)
      -> delete:(unit -> 'r)
      -> 'r
  end = struct
    type one_field_change =
      { change : Syntax.change
      ; new_name : string option
      ; index : int option
      ; value : Sexp.t
      }

    (* some constructors below were useful for incrementally defunctionalizing the code
       with help from the type checker. Even after this process is complete, they are
       still useful as type-checked documentation to indicate what function type the
       datatypes correspond to. We mark these scaffolding constructors with an uninhabited
       type named [_unused] so that the type-checker also enforces that the constructors
       are not actually used at run-time. *)
    type _unused

    type 'r seq =
      (*=| SEQ of ('r cont -> 'r) * _unused*)
      | Nil_seq
      | Suspend of Query.t * Sexp.t
      | App of 'r seq * 'r seq
      | Of_list of Sexp.t list
      | Smash_all of Sexp.t list
      | Recons of Sexp.t * 'r seq
      | Apply_bindf of Sexp.t * 'r bindf
      | Bind of 'r seq * 'r bindf
      | Apply_bindf'_ of Sexp.t list * 'r bindf'_
      | Bind'_ of 'r seq' * 'r bindf'_
      | Bind_rows_wrap of Sexp.t * Syntax.Query.t Syntax.anti_quote Template.t list

    and 'r cont =
      | CONT of ('r node -> 'r)
      (* only used once *)
      | App_body of 'r seq * 'r cont
      | Bind_body of 'r bindf * 'r cont
      | Bind_'body of 'r bindf_' * 'r cont'
      | To_list_body of (Sexp.t list -> 'r)
      | If_body of Sexp.t * Syntax.Query.t * Syntax.Query.t * 'r cont
      | Branch_body of Sexp.t * Syntax.Query.t * Syntax.Query.t * 'r cont
      | And_body of Sexp.t * Syntax.Query.t * 'r cont
      | Or_body of Sexp.t * Syntax.Query.t * 'r cont
      | Test_body of Sexp.t * 'r cont
      | Not_body of Sexp.t * 'r cont
      | Gather of (Sexp.t -> 'r)

    and 'r node =
      | Nil
      | Cons of Sexp.t * 'r seq

    and 'r seq' =
      (*=| SEQ' of ('r cont' -> 'r) * _unused*)
      | Nil_seq'
      | App' of 'r seq' * 'r seq'
      | Rows of Sexp.t * Syntax.Query.t Syntax.anti_quote Template.t list
      | Thunk of 'r cache ref
      | Apply_bindf_' of Sexp.t * 'r bindf_'
      | Bind_' of 'r seq * 'r bindf_'
      | Map of 'r seq' * 'r mapf'

    and 'r cont' =
      (*=| CONT' of ('r node' -> 'r) * _unused*)
      | Remember of 'r cache ref * 'r cont'
      | App'_body of 'r seq' * 'r cont'
      | Bind'_body of 'r bindf'_ * 'r cont
      | Map'_body of 'r mapf' * 'r cont'

    and 'r node' =
      | Nil'
      | Cons' of Sexp.t list * 'r seq'

    and 'r cache =
      | Evaluated of 'r node'
      | Unevaluated of 'r seq'

    and 'r bindf =
      (*=| BINDF of (Sexp.t -> 'r cont -> 'r) * _unused*)
      | Smash
      | Query of Query.t
      | Extract_field of string

    and 'r bindf'_ = (*=| BINDF'_ of (Sexp.t list -> 'r cont -> 'r) * _unused*)
      | Wrap

    and 'r bindf_' =
      (*=| BINDF_' of (Sexp.t -> 'r cont' -> 'r) * _unused*)
      | Prepend_col of 'r seq'

    and 'r mapf' =
      (*=| MAPF' of (Sexp.t list -> Sexp.t list) * _unused*)
      | Prepend_one of Sexp.t
      | Prepend_row of Sexp.t list

    let rec apply_seq code k =
      match code with
      (*=| SEQ (f, _never_fires) -> f k*)
      | Nil_seq -> nil k
      | Suspend (q, s) -> query q s k
      | App (m', n) -> app m' n k
      | Of_list ys -> of_list ys k
      | Smash_all ss -> bind (Of_list ss) ~f:Smash k
      | Recons (x, xs) -> apply_cont k (Cons (x, xs))
      | Apply_bindf (x, f) -> apply_bindf f x k
      | Bind (xs, f) -> bind xs ~f k
      | Apply_bindf'_ (x, f) -> apply_bindf'_ f x k
      | Bind'_ (xs, f) -> bind'_ xs f k
      | Bind_rows_wrap (s, ts) -> bind'_ (Rows (s, ts)) Wrap k

    and apply_cont code node =
      match code with
      | CONT f -> f node
      | App_body (n, k) ->
        (match node with
         | Nil -> apply_seq n k
         | Cons (x, m') -> apply_cont k (Cons (x, App (m', n))))
      | Bind_body (f, k) ->
        (match node with
         | Nil -> apply_cont k Nil
         | Cons (x, xs) -> app (Apply_bindf (x, f)) (Bind (xs, f)) k)
      | Bind_'body (f, k) ->
        (match node with
         | Nil -> apply_cont' k Nil'
         | Cons (x, xs) -> app' (Apply_bindf_' (x, f)) (Bind_' (xs, f)) k)
      | To_list_body k ->
        (match node with
         | Nil -> k []
         | Cons (y, ys) -> to_list ys (fun ys -> k (y :: ys)))
      | If_body (s, q2, q3, k) ->
        (match node with
         | Cons (_, _) -> query q2 s k
         | Nil -> query q3 s k)
      | Branch_body (s, q2, q3, k) ->
        (match node with
         | Cons (x, xs) -> bind (Recons (x, xs)) ~f:(Query q2) k
         | Nil -> query q3 s k)
      | And_body (s, q2, k) ->
        (match node with
         | Cons (_, _) -> query q2 s k
         | Nil -> nil k)
      | Or_body (s, q2, k) ->
        (match node with
         | Cons (x, xs) -> apply_cont k (Cons (x, xs))
         | Nil -> query q2 s k)
      | Test_body (s, k) ->
        (match node with
         | Cons (_, _) -> one s k
         | Nil -> nil k)
      | Not_body (s, k) ->
        (match node with
         | Cons (_, _) -> nil k
         | Nil -> one s k)
      | Gather diff ->
        (match node with
         | Nil -> diff (List []) (* or [fail ()] *)
         | Cons (x, v) -> to_list v (fun xs -> diff (List (x :: xs))))

    and apply_bindf code x k =
      match code with
      (*=| BINDF (f, _never_fires) -> f x k*)
      | Smash -> smash x k (* faster version of [Query Smash] *)
      | Query q -> query q x k
      | Extract_field name ->
        (match x with
         | List [ Atom fname; fvalue ] when String.( = ) fname name -> one fvalue k
         | _ -> nil k)

    and apply_bindf'_ code row k =
      match code with
      (*=| BINDF'_ (f, _never_fires) -> f row k*)
      | Wrap -> one (List row) k

    and apply_bindf_' code x k =
      match code with
      (*=| BINDF_' (f, _never_fires) -> f x k*)
      | Prepend_col blocks -> map' blocks ~f:(Prepend_one x) k

    and apply_seq' code k =
      match code with
      (*=| SEQ' (f, _never_fires) -> f k*)
      | Nil_seq' -> nil' k
      | App' (xs, n) -> app' xs n k
      | Rows (s, ts) -> rows s ts k
      | Apply_bindf_' (x, f) -> apply_bindf_' f x k
      | Bind_' (xs, f) -> bind_' xs f k
      | Map (xs, f) -> map' xs ~f k
      | Thunk cache ->
        (match !cache with
         | Evaluated v -> apply_cont' k v
         | Unevaluated m -> apply_seq' m (Remember (cache, k)))

    and apply_cont' code node =
      match code with
      (*=| CONT' (f, _never_fires) -> f node*)
      | Remember (cache, k) ->
        cache := Evaluated node;
        apply_cont' k node
      | App'_body (n, k) ->
        (match node with
         | Nil' -> apply_seq' n k
         | Cons' (x, xs) -> apply_cont' k (Cons' (x, App' (xs, n))))
      | Bind'_body (f, k) ->
        (match node with
         | Nil' -> apply_cont k Nil
         | Cons' (x, xs) -> app (Apply_bindf'_ (x, f)) (Bind'_ (xs, f)) k)
      | Map'_body (f, k) ->
        (match node with
         | Nil' -> apply_cont' k Nil'
         | Cons' (x, xs) ->
           let y = apply_mapf' f x in
           apply_cont' k (Cons' (y, Map (xs, f))))

    and apply_mapf' code ys =
      match code with
      (*=| MAPF' (f, _never_fires) -> f ys*)
      | Prepend_one x -> x :: ys
      | Prepend_row xs -> xs @ ys

    and nil k = apply_cont k Nil
    and nil' k = apply_cont' k Nil'
    and one s k = apply_cont k (Cons (s, Nil_seq))
    and app m n k = apply_seq m (App_body (n, k))
    and app' m n k = apply_seq' m (App'_body (n, k))
    and bind m ~f k = apply_seq m (Bind_body (f, k))
    and bind_' m f k = apply_seq m (Bind_'body (f, k))
    and bind'_ m f k = apply_seq' m (Bind'_body (f, k))
    and map' m ~f k = apply_seq' m (Map'_body (f, k))

    and atomic s k =
      match s with
      | Atom _ as s -> one s k
      | List _ -> nil k

    and length s k =
      match s with
      | Atom _ -> one ([%sexp_of: int] 1) k
      | List l -> one ([%sexp_of: int] (List.length l)) k

    and index i s k =
      match s with
      | Atom _ -> nil k
      | List xs ->
        let i = if i < 0 then List.length xs + i else i in
        (match List.nth xs i with
         | None -> nil k
         | Some s -> one s k)

    and of_list xs k =
      match xs with
      | [] -> apply_seq Nil_seq k
      | y :: ys -> apply_cont k (Cons (y, Of_list ys))

    and to_list xs k = apply_seq xs (To_list_body k)

    and smash s k =
      match s with
      | Atom _ -> one s k
      | List ss -> apply_cont k (Cons (s, Smash_all ss))

    and each s k =
      match s with
      | Atom _ -> nil k
      | List ss -> of_list ss k

    and field name s k =
      match s with
      | Atom _ -> nil k
      | List ss -> bind (Of_list ss) ~f:(Extract_field name) k

    and variant name n_opt s k =
      match s with
      | Atom cname when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = 0 then one s k else nil k)
      | List (Atom cname :: args) when String.( = ) cname name ->
        (match n_opt with
         | None -> one s k
         | Some n -> if n = List.length args then one s k else nil k)
      | _ -> nil k

    and list_map xs ~f k =
      match xs with
      | [] -> k []
      | x :: xs -> f x (fun x -> list_map xs ~f (fun xs -> k (x :: xs)))

    and atom_map ~f s k =
      match s with
      | Atom x -> k (Atom (f x))
      | List ss -> list_map ss ~f:(fun s k -> atom_map ~f s k) (fun ss -> k (List ss))

    and memo m = Thunk (ref (Unevaluated m))

    and regex r s k =
      match s with
      | Atom s ->
        if Re2.num_submatches r > 1
        then (
          try one (Atom (Re2.find_first_exn ~sub:(`Index 1) r s)) k with
          | _ -> nil k)
        else if Re2.matches r s
        then one (Atom s) k
        else nil k
      | List _ -> nil k

    and query q s k =
      match q with
      | Syntax.This -> one s k
      | Syntax.Die -> nil k
      | Syntax.Length -> length s k
      | Syntax.Atomic -> atomic s k
      | Syntax.Index i -> index i s k
      | Syntax.Smash -> smash s k
      | Syntax.Pipe (q1, q2) -> bind (Suspend (q1, s)) ~f:(Query q2) k
      | Syntax.Cat (q1, q2) -> app (Suspend (q1, s)) (Suspend (q2, s)) k
      | Syntax.Each -> each s k
      | Syntax.Field f -> field f s k
      | Syntax.Variant (c, n) -> variant c n s k
      | Syntax.Equals s' -> if Hash_set.mem s' s then one s k else nil k
      | Syntax.Regex r -> regex r s k
      | Syntax.If (q1, q2, q3) -> query q1 s (If_body (s, q2, q3, k))
      | Syntax.Branch (q1, q2, q3) -> query q1 s (Branch_body (s, q2, q3, k))
      | Syntax.And (q1, q2) -> query q1 s (And_body (s, q2, k))
      | Syntax.Or (q1, q2) -> query q1 s (Or_body (s, q2, k))
      | Syntax.Wrap q -> to_list (Suspend (q, s)) (fun xs -> one (List xs) k)
      | Syntax.Test q -> query q s (Test_body (s, k))
      | Syntax.Not q -> query q s (Not_body (s, k))
      | Syntax.Quote t ->
        quote
          t
          s
          ~dot:(fun x -> one x k)
          ~row:(fun xs -> of_list xs k)
          ~col:(fun xs -> apply_seq xs k)
      | Syntax.Change c ->
        change
          c
          s
          ~fail:(fun () -> nil k)
          ~delete:(fun () -> nil k)
          ~same:(fun () -> one s k)
          ~diff:(fun s -> one s k)
      | Syntax.Restructure -> restructure k s ~nil ~of_list

    and quote t s ~dot ~col ~row =
      match t with
      | Template.Atom a -> dot (Atom a)
      | Template.Hole (Syntax.Unquote q) -> col (Suspend (q, s))
      | Template.Hole (Syntax.Splice q) -> to_list (Suspend (q, s)) row
      | Template.List ts -> col (Bind_rows_wrap (s, ts))

    and rows s ts k =
      match ts with
      | [] -> apply_cont' k (Cons' ([], Nil_seq'))
      | t :: ts ->
        let blocks = Rows (s, ts) in
        quote
          t
          s
          ~dot:(fun x -> map' blocks ~f:(Prepend_one x) k)
          ~row:(fun xs -> map' blocks ~f:(Prepend_row xs) k)
          ~col:(fun xs -> bind_' xs (Prepend_col (memo blocks)) k)

    and change c s ~fail ~same ~diff ~delete =
      match c with
      | Syntax.Id -> same ()
      | Syntax.Fail -> fail ()
      | Syntax.Delete -> delete ()
      | Syntax.Seq (c1, c2) ->
        change
          c1
          s
          ~fail
          ~same:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~diff:(fun s -> change c2 s ~fail ~same:(fun () -> diff s) ~diff ~delete)
          ~delete
      | Syntax.Alt (c1, c2) ->
        change
          c1
          s
          ~same
          ~diff
          ~fail:(fun () -> change c2 s ~fail ~same ~diff ~delete)
          ~delete
      | Syntax.Topdown c -> change (Unroll.topdown c) s ~fail ~same ~diff ~delete
      | Syntax.Bottomup c -> change (Unroll.bottomup c) s ~fail ~same ~diff ~delete
      | Syntax.Children c ->
        (match s with
         | Atom _ -> same ()
         | List ss -> children c ss ~fail ~same ~diff:(fun ss -> diff (List ss)))
      | Syntax.Record field_changes ->
        (match sexp_to_record s with
         | None -> fail ()
         | Some fields ->
           let module F = Syntax.Record_field in
           let default_field_change =
             Option.value
               (Map.find field_changes "_")
               ~default:{ F.change = Syntax.Id; new_name = None; presence = `Optional }
           in
           let field_changes = Map.remove field_changes "_" in
           let field_changes =
             with_return (fun r ->
               Some
                 (Map.to_alist
                    (Map.merge field_changes fields ~f:(fun ~key:_ lr ->
                       let { F.change; new_name; presence }, iv =
                         match lr with
                         | `Left f -> f, None
                         | `Right iv -> default_field_change, Some iv
                         | `Both (f, iv) -> f, Some iv
                       in
                       (match presence, is_some iv with
                        | `Present, false | `Absent, true -> r.return None
                        | (`Present | `Optional | `Absent), _ -> ());
                       let index, value =
                         match iv with
                         | None -> None, Sexp.List []
                         | Some (index, value) -> Some index, value
                       in
                       Some { change; new_name; value; index }))))
           in
           (match field_changes with
            | None -> fail ()
            | Some field_changes -> record ~field_changes ~fail ~diff ~accum:[]))
      | Syntax.Rewrite (lhs, rhs) ->
        Pattern.pmatch lhs s ~fail ~succ:(fun env -> Pattern.instantiate rhs env diff)
      | Syntax.Rewrite_record (lhs, rhs) ->
        Pattern_record.pmatch lhs s ~fail ~succ:(fun env ->
          Pattern_record.instantiate rhs env diff)
      | Syntax.Lowercase -> atom_map s ~f:String.lowercase diff
      | Syntax.Concat ->
        (match s with
         | Atom _ -> same ()
         | List _ -> diff (Atom (sexp_collapse s)))
      | Syntax.Query q -> query q s (Gather diff)

    and children c ss ~fail ~same ~diff =
      match ss with
      | [] -> same ()
      | s :: ss ->
        change
          c
          s
          ~fail
          ~same:(fun () -> children c ss ~fail ~same ~diff:(fun ss -> diff (s :: ss)))
          ~diff:(fun s ->
            children
              c
              ss
              ~fail
              ~same:(fun () -> diff (s :: ss))
              ~diff:(fun ss -> diff (s :: ss)))
          ~delete:(fun () -> children c ss ~fail ~same:(fun () -> diff ss) ~diff)

    and record ~field_changes ~fail ~diff ~accum =
      match field_changes with
      | [] ->
        let alist =
          List.map
            ~f:(fun (_, field, value) -> field, value)
            (List.sort accum ~compare:(fun (index, field, _) (index', field', _) ->
               match index, index' with
               | None, None -> String.compare field field'
               | None, Some _ -> 1
               | Some _, None -> -1
               | Some i, Some i' -> Int.compare i i'))
        in
        if List.contains_dup alist ~compare:(fun (field, _) (field', _) ->
             String.compare field field')
        then fail ()
        else
          diff
            (List (List.map alist ~f:(fun (field, value) -> List [ Atom field; value ])))
      | (field, { change = c; new_name; index; value }) :: field_changes ->
        let continue accum = record ~field_changes ~fail ~diff ~accum in
        let diff value =
          continue ((index, Option.value new_name ~default:field, value) :: accum)
        in
        change
          c
          value
          ~fail
          ~same:(fun () -> diff value)
          ~diff
          ~delete:(fun () -> continue accum)
    ;;

    let pull seq k = apply_seq seq (CONT k)
    let query q s = Suspend (q, s)
  end

  type state = State of state Internal.node

  let query q s =
    Lazy_list.build ~seed:(Internal.query q s) ~f:(fun sexps ->
      match Internal.pull sexps (fun x -> State x) with
      | State Internal.Nil -> None
      | State (Internal.Cons (x, v)) -> Some (x, v))
  ;;

  let change c s =
    Internal.change
      c
      s
      ~fail:(fun () -> None)
      ~delete:(fun () -> None)
      ~same:(fun () -> Some s)
      ~diff:(fun s -> Some s)
  ;;
end

include Nofun

(* default implementation *)

let query' q x = query q (Sexp_ext.sexp_of_t x)

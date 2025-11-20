open Core

type regex = Re2.Stable.V1_no_options.t [@@deriving sexp]

type sexp = Sexp.t =
  | Atom of string
  | List of sexp list

(* cheap import *)

let con x t = List [ Atom x; t ]

exception Escaped_literal of string [@@deriving sexp]

module Template = struct
  type 'a t =
    | Hole of 'a
    | Atom of string
    | List of 'a t list

  let con x t = List [ Atom x; t ]

  let rec map t ~f =
    match t with
    | Atom s -> Atom s
    | List ts -> List (List.map ~f:(map ~f) ts)
    | Hole a -> Hole (f a)
  ;;

  let rec literal = function
    | Sexp.Atom x -> Atom x
    | Sexp.List xs -> List (List.map ~f:literal xs)
  ;;

  let rec sexp_of_t sexp_of_a = function
    | Atom s -> Sexp.Atom s
    | Hole a -> sexp_of_a a
    | List ts -> Sexp.List (List.map ~f:(sexp_of_t sexp_of_a) ts)
  ;;

  let rec t_of_sexp a_of_sexp sexp =
    try Hole (a_of_sexp sexp) with
    | Escaped_literal s | Of_sexp_error (Escaped_literal s, _) -> Atom s
    | _ ->
      (match sexp with
       | Sexp.Atom s -> Atom s
       | Sexp.List xs -> List (List.map ~f:(t_of_sexp a_of_sexp) xs))
  ;;
end

module Quotation = struct
  type 'a t =
    | Atom of string
    | List of 'a t list
    | Quote of 'a t t
    | Unquote of 'a
    | Splice of 'a

  (* recursive record needed here to account for polymorphic recursion *)
  module rec X : sig
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
    val flatten' : ('a -> 'b Template.t) -> 'a t -> 'b Template.t
  end = struct
    let rec sexp_of_t sexp_of_a = function
      | Atom s -> Sexp.Atom s
      | Quote t -> con "quote" (X.sexp_of_t (sexp_of_t sexp_of_a) t)
      | Unquote a -> con "unquote" (sexp_of_a a)
      | Splice a -> con "splice" (sexp_of_a a)
      | List ts -> Sexp.List (List.map ~f:(sexp_of_t sexp_of_a) ts)
    ;;

    let rec t_of_sexp a_of_sexp = function
      | Sexp.Atom s -> Atom s
      | Sexp.List [ Sexp.Atom "quote"; x ] -> Quote (X.t_of_sexp (t_of_sexp a_of_sexp) x)
      | Sexp.List [ Sexp.Atom "unquote"; x ] -> Unquote (a_of_sexp x)
      | Sexp.List [ Sexp.Atom "splice"; x ] -> Splice (a_of_sexp x)
      | Sexp.List xs -> List (List.map ~f:(t_of_sexp a_of_sexp) xs)
    ;;

    let rec flatten' flatten = function
      | Atom s -> Template.Atom s
      | Quote t -> Template.con "quote" (X.flatten' (flatten' flatten) t)
      | Unquote a -> Template.con "unquote" (flatten a)
      | Splice a -> Template.con "splice" (flatten a)
      | List ts -> Template.List (List.map ~f:(flatten' flatten) ts)
    ;;
  end

  include X
end

type 'a anti_quote =
  | Unquote of 'a
  | Splice of 'a
[@@deriving sexp]

let rec flatten = function
  | Quotation.Atom s -> Template.Atom s
  | Quotation.Unquote a -> Template.Hole (Unquote a)
  | Quotation.Splice a -> Template.Hole (Splice a)
  | Quotation.List ts -> Template.List (List.map ~f:flatten ts)
  | Quotation.Quote t -> Template.con "quote" (Quotation.flatten' flatten t)
;;

module Var = struct
  module T = struct
    type t = string [@@deriving compare, bin_io, hash]

    let prefixes = [ '$'; '@' ]
    let invariant t = String.length t > 0 && List.mem prefixes t.[0] ~equal:Char.equal

    let of_string x =
      (* Special case to allow "$$100" as an escaped version of "$100". *)
      if invariant x
      then
        if String.length x > 1 && Char.( = ) x.[0] x.[1]
        then raise (Escaped_literal (String.drop_prefix x 1))
        else x
      else
        invalid_argf
          "Pattern var %s does not have the required prefix %s"
          x
          (List.map prefixes ~f:String.of_char |> String.concat ~sep:" ")
          ()
    ;;

    let to_string t =
      assert (invariant t);
      t
    ;;
  end

  include T

  include Identifiable.Make (struct
      let module_name = "Sexp_app.Syntax.Var"

      include T
      include Sexpable.Of_stringable (T)
    end)

  let is_list t = Char.( = ) t.[0] '@'
end

module Pattern_general = struct
  type t = Var.t Template.t [@@deriving sexp]

  let var_tally pat =
    let tally = Var.Table.create ~size:10 () in
    let count var = incr (Hashtbl.find_or_add tally var ~default:(fun () -> ref 0)) in
    let rec count_all = function
      | [] -> tally
      | pat :: pats ->
        let pats =
          match pat with
          | Template.Hole var ->
            count var;
            pats
          | Template.Atom _ -> pats
          | Template.List pats' -> List.rev_append pats' pats
        in
        count_all pats
    in
    count_all [ pat ]
  ;;

  let check_rewrite_scope ~lhs ~rhs =
    let lhs_tally = var_tally lhs in
    let rhs_tally = var_tally rhs in
    Hashtbl.iteri lhs_tally ~f:(fun ~key:lhs_var ~data:count ->
      if !count > 1
      then
        failwithf
          "variable %s bound multiple times in LHS pattern"
          (Var.to_string lhs_var)
          ());
    Hashtbl.iteri rhs_tally ~f:(fun ~key:rhs_var ~data:_ ->
      if not (Hashtbl.mem lhs_tally rhs_var)
      then failwithf "unbound variable %s in RHS pattern" (Var.to_string rhs_var) ())
  ;;

  let check_duplicate_list_vars ~lhs =
    let rec duplicate_list_vars = function
      | Template.List l ->
        let list_vars =
          List.filter_map l ~f:(function
            | Template.Hole var -> if Var.is_list var then Some var else None
            | _ -> None)
        in
        if List.length list_vars > 1
        then list_vars
        else List.map l ~f:duplicate_list_vars |> List.concat
      | _ -> []
    in
    match duplicate_list_vars lhs with
    | [] -> ()
    | duplicates ->
      failwithf
        "duplicate list variables %s in the same list"
        (List.map duplicates ~f:Var.to_string |> String.concat ~sep:" ")
        ()
  ;;

  let rec instantiate t env k =
    match t with
    | Template.List ts -> instantiate_all ts env (fun ts -> k (List ts))
    | Template.Atom x -> k (Atom x)
    | Template.Hole v ->
      (match Hashtbl.find env v with
       | Some s -> k s
       | None -> assert false)

  (* error in [Syntax.check_rewrite_scope] *)
  and instantiate_all ts env k =
    match ts with
    | [] -> k []
    | t :: ts ->
      let cont () =
        instantiate t env (fun t -> instantiate_all ts env (fun ts -> k (t :: ts)))
      in
      (match t with
       | Template.Hole v ->
         if Var.is_list v
         then (
           match Hashtbl.find env v with
           | None -> assert false (* error in [Syntax.check_rewrite_scope] *)
           | Some s ->
             (match s with
              | List s -> instantiate_all ts env (fun ts -> k (s @ ts))
              | _ -> assert false))
         else cont ()
       | _ -> cont ())
  ;;
end

module type Pattern_general = sig
  type t = Var.t Template.t [@@deriving sexp]

  val pmatch : t -> Sexp.t -> fail:(unit -> 'a) -> succ:(Sexp.t Var.Table.t -> 'a) -> 'a
  val instantiate : t -> Sexp.t Var.Table.t -> (Sexp.t -> 'b) -> 'b
end

module Pattern = struct
  include Pattern_general

  let pmatch t s ~fail ~succ =
    let env = Var.Table.create ~size:10 () in
    let assign v s =
      match Hashtbl.find env v with
      | Some _ -> assert false (* error in [Syntax.check_rewrite_scope] *)
      | None -> Hashtbl.set env ~key:v ~data:s
    in
    let rec gather t ~fail ~succ =
      match t with
      | Template.Hole v, s ->
        assign v s;
        succ ()
      | Template.Atom a, Atom b -> if String.( = ) a b then succ () else fail ()
      | Template.List ts, List xs ->
        let rec gather_all txs ~rev ~fail ~succ =
          match txs with
          | t :: ts, x :: xs ->
            let cont () =
              gather (t, x) ~fail ~succ:(fun () -> gather_all (ts, xs) ~rev ~fail ~succ)
            in
            (match t with
             | Template.Hole v ->
               if Var.is_list v
               then (
                 match ts with
                 | [] ->
                   assign v (List (if rev then List.rev (x :: xs) else x :: xs));
                   gather_all (ts, []) ~rev ~fail ~succ
                 | _ :: _ ->
                   (* This assertion failing means that we have two list variables in the
                      template. This should be prevented by [check_duplicate_list_vars]. *)
                   assert (not rev);
                   gather_all
                     (List.rev (t :: ts), List.rev (x :: xs))
                     ~rev:(not rev)
                     ~fail
                     ~succ)
               else cont ()
             | _ -> cont ())
          | [], [] -> succ ()
          | _ -> fail ()
        in
        gather_all (ts, xs) ~rev:false ~fail ~succ
      | Template.List _, Atom _ | Template.Atom _, List _ -> fail ()
    in
    gather (t, s) ~fail ~succ:(fun () -> succ env)
  ;;
end

module Pattern_record = struct
  include Pattern_general

  let pmatch t s ~fail ~succ =
    let assign env v s =
      match Hashtbl.find env v with
      | Some _ -> assert false (* error in [Syntax.check_rewrite_scope] *)
      | None -> Hashtbl.set env ~key:v ~data:s
    in
    let rec gather t env =
      (* Matches t with any given xs. If successful, returns the environment and the
         remaining xs. *)
      let rec match_any t acc = function
        | [] -> None
        | x :: xs ->
          let local_env = Var.Table.create ~size:10 () in
          if gather (t, x) local_env
          then Some (local_env, acc @ xs)
          else match_any t (x :: acc) xs
      in
      (* Matches each given t to any of the given xs. If successful, returns the unmatched
         xs. *)
      let rec match_each xs = function
        | [] -> Some xs
        | t :: ts ->
          (match match_any t [] xs with
           | None -> None
           | Some (local_env, xs) ->
             Hashtbl.iteri local_env ~f:(fun ~key:v ~data:s -> assign env v s);
             match_each xs ts)
      in
      match t with
      | Template.Hole v, s ->
        if Var.is_list v
        then false
        else (
          assign env v s;
          true)
      | Template.Atom a, Atom b -> if String.( = ) a b then true else false
      | Template.List ts, List xs ->
        (* Each of the ts should match first x from xs which it can match. If there's a
           list variable among ts, all the unmatched xs will be stored in it. *)
        let list_vars, ts =
          List.partition_tf ts ~f:(function
            | Template.Hole v -> Var.is_list v
            | _ -> false)
        in
        (match match_each xs ts with
         | None -> false
         | Some xs ->
           (match list_vars with
            | [] -> List.is_empty xs
            | [ Template.Hole list_var ] ->
              assign env list_var (List xs);
              true
            | _ -> assert false))
      (* error in [Syntax.check_duplicate_list_vars] *)
      | Template.List _, Atom _ | Template.Atom _, List _ -> false
    in
    let env = Var.Table.create ~size:10 () in
    if gather (t, s) env then succ env else fail ()
  ;;
end

module Record_field = struct
  type 'change t =
    { change : 'change
    ; new_name : string option
    ; presence : [ `Present | `Optional | `Absent ]
    }
  [@@deriving sexp]
end

type query =
  (* KEEP THIS TYPE IN SYNC WITH THE MODULE [Grammar] *)
  | This
  | Pipe of query * query
  | Die
  | Cat of query * query
  | Equals of Sexps.t
  | Regex of regex
  | Variant of string * int option
  | Field of string
  | Index of int
  | Each
  | Smash
  | Atomic
  | Length
  | Wrap of query
  | Test of query
  | Not of query
  | And of query * query
  | Or of query * query
  | If of query * query * query
  | Branch of query * query * query
  | Quote of query anti_quote Template.t
  | Change of change
  | Restructure

and change =
  (* KEEP THIS TYPE IN SYNC WITH THE MODULE [Grammar] *)
  | Id
  | Fail
  | Delete
  | Alt of change * change
  | Seq of change * change
  | Children of change
  | Record of change Record_field.t String.Map.t
  | Rewrite of Pattern.t * Pattern.t
  | Rewrite_record of Pattern_record.t * Pattern_record.t
  | Topdown of change
  | Bottomup of change
  | Lowercase
  | Concat
  | Query of query
[@@deriving sexp]

let pipe = function
  | [] -> This
  | q :: qs -> List.fold qs ~init:q ~f:(fun q1 q2 -> Pipe (q1, q2))
;;

let cat = function
  | [] -> Die
  | q :: qs -> List.fold qs ~init:q ~f:(fun q1 q2 -> Cat (q1, q2))
;;

let and_ = function
  | [] -> This
  | q :: qs -> List.fold qs ~init:q ~f:(fun q1 q2 -> And (q1, q2))
;;

let or_ = function
  | [] -> Die
  | q :: qs -> List.fold qs ~init:q ~f:(fun q1 q2 -> Or (q1, q2))
;;

let seq = function
  | [] -> Id
  | c :: cs -> List.fold cs ~init:c ~f:(fun c1 c2 -> Seq (c1, c2))
;;

let alt = function
  | [] -> Fail
  | c :: cs -> List.fold cs ~init:c ~f:(fun c1 c2 -> Alt (c1, c2))
;;

let quote q = Quote (flatten q)
let try_ c = Alt (c, Id)
let equals s = Equals (Sexps.of_list [ s ])

module Unroll = struct
  let topdown c = Seq (c, Children (Topdown c))
  let bottomup c = Seq (Children (Bottomup c), c)
end

let rewrite ~lhs ~rhs =
  Pattern.check_rewrite_scope ~lhs ~rhs;
  Pattern.check_duplicate_list_vars ~lhs;
  Rewrite (lhs, rhs)
;;

let rewrite_record ~lhs ~rhs =
  Pattern_record.check_rewrite_scope ~lhs ~rhs;
  Pattern_record.check_duplicate_list_vars ~lhs;
  Rewrite_record (lhs, rhs)
;;

let const sexp =
  rewrite ~lhs:(Template.Hole (Var.of_string "$_")) ~rhs:(Template.literal sexp)
;;

let deprecated ~command ~suggestion =
  let only_run_once =
    lazy (eprintf "The '%s' command is now deprecated. %s\n" command suggestion)
  in
  fun () -> Lazy.force only_run_once
;;

let lowercase_deprecated =
  deprecated ~command:"lowercase" ~suggestion:"use '(change lowercase)' instead"
;;

(* custom sexp conversion *)
let old_query_of_sexp = query_of_sexp
let old_change_of_sexp = change_of_sexp

let rec query_of_sexp = function
  | List (Atom "pipe" :: xs) -> pipe (List.map ~f:query_of_sexp xs)
  | List (Atom "cat" :: xs) -> cat (List.map ~f:query_of_sexp xs)
  | List (Atom "and" :: xs) -> and_ (List.map ~f:query_of_sexp xs)
  | List (Atom "or" :: xs) -> or_ (List.map ~f:query_of_sexp xs)
  | List (Atom "test" :: xs) ->
    (* sugar *)
    Test (pipe (List.map ~f:query_of_sexp xs))
  | List [ Atom "not"; x ] -> Not (query_of_sexp x)
  | List [ Atom "wrap"; x ] -> Wrap (query_of_sexp x)
  | List [ Atom "quote"; x ] -> quote (Quotation.t_of_sexp query_of_sexp x)
  | List [ Atom "if"; x; y; z ] -> If (query_of_sexp x, query_of_sexp y, query_of_sexp z)
  | List [ Atom "branch"; x; y; z ] ->
    Branch (query_of_sexp x, query_of_sexp y, query_of_sexp z)
  | List [ Atom "variant"; Atom name ] ->
    (* sugar *)
    Variant (name, None)
  | List [ Atom "variant"; Atom name; Atom arity ] ->
    Variant (name, Some (Int.of_string arity))
  | List (Atom "equals" :: (_ :: _ as sexps)) -> Equals (Sexps.of_list sexps)
  | List [ Atom "change"; change ] -> Change (change_of_sexp change)
  | Atom "none" -> Die
  | Atom "lowercase" ->
    lowercase_deprecated ();
    Change Lowercase
  | other -> old_query_of_sexp other

and change_of_sexp = function
  | List [ Atom "children"; x ] -> Children (change_of_sexp x)
  | List [ Atom "try"; x ] -> try_ (change_of_sexp x)
  | List [ Atom "topdown"; x ] -> Topdown (change_of_sexp x)
  | List [ Atom "bottomup"; x ] -> Bottomup (change_of_sexp x)
  | List [ Atom "rewrite"; lhs; rhs ] ->
    rewrite ~lhs:(Pattern.t_of_sexp lhs) ~rhs:(Pattern.t_of_sexp rhs)
  | List [ Atom "rewrite_record"; lhs; rhs ] ->
    rewrite_record ~lhs:(Pattern_record.t_of_sexp lhs) ~rhs:(Pattern_record.t_of_sexp rhs)
  | List (Atom "alt" :: xs) -> alt (List.map ~f:change_of_sexp xs)
  | List (Atom "seq" :: xs) -> seq (List.map ~f:change_of_sexp xs)
  | List (Atom "record" :: fields) as sexp ->
    let of_sexp_error = Sexplib.Conv.of_sexp_error in
    let fields =
      List.map fields ~f:(function
        | List (Atom field :: rest) as sexp ->
          let options, change =
            let options = [] in
            let change = Atom "id" in
            match rest with
            | [] -> options, change
            | [ change ] -> options, change
            | [ List options; change ] -> options, change
            | _ -> of_sexp_error "invalid record field" sexp
          in
          let new_name = ref None in
          let presence = ref `Present in
          List.iter options ~f:(function
            | Atom "present" -> presence := `Present
            | Atom "absent" -> presence := `Absent
            | Atom "optional" -> presence := `Optional
            | List [ Atom "rename"; Atom a ] -> new_name := Some a
            | sexp -> of_sexp_error "invalid record field option" sexp);
          ( field
          , { Record_field.presence = !presence
            ; new_name = !new_name
            ; change = change_of_sexp change
            } )
        | sexp -> of_sexp_error "invalid record field" sexp)
    in
    (match String.Map.of_alist fields with
     | `Ok map -> Record map
     | `Duplicate_key field ->
       Sexplib.Conv.of_sexp_error (sprintf "duplicate record field '%s'" field) sexp)
  | List [ Atom "const"; x ] -> const x
  | List [ Atom "query"; query ] -> Query (query_of_sexp query)
  | other -> old_change_of_sexp other
;;

module Query = struct
  type t = query [@@deriving sexp]
end

module Change = struct
  type t = change [@@deriving sexp]
end

open Core
open Poly

(* This module type is so that we can write the code for a single engine that works both
   on plain Sexp.t and a version of Sexp.t where the nodes are labeled with an integer, by
   providing a common interface for the engine to work with. *)
module type Sexplike = sig
  type t

  type unwrapped =
    | Atom of string
    | List of t list

  val unwrap : t -> unwrapped
end

module Sexp = struct
  type unwrapped = Sexp.t =
    | Atom of string
    | List of Sexp.t list

  include Sexp

  let unwrap = Fn.id
end

module Labeled_sexp = struct
  type unwrapped =
    | Atom of string
    | List of t list

  and t =
    { unwrapped : unwrapped
    ; node_id : int
    }

  let unwrap t = t.unwrapped

  let of_sexp sexp =
    let node_id = ref (-1) in
    let get_node_id () =
      incr node_id;
      !node_id
    in
    let rec of_sexp sexp =
      match sexp with
      | Sexp.Atom s -> { unwrapped = Atom s; node_id = get_node_id () }
      | Sexp.List list ->
        let node_id = get_node_id () in
        let children = List.map list ~f:of_sexp in
        { unwrapped = List children; node_id }
    in
    of_sexp sexp
  ;;

  let rec to_sexp t =
    match t.unwrapped with
    | Atom s -> Sexp.Atom s
    | List list -> Sexp.List (List.map list ~f:to_sexp)
  ;;

  let rec to_sexp_with_replacements t ~f =
    match f t.node_id with
    | Some replacement -> replacement
    | None ->
      (match t.unwrapped with
       | Atom s -> Sexp.Atom s
       | List list -> Sexp.List (List.map list ~f:(to_sexp_with_replacements ~f)))
  ;;
end

module Make_engine (S : Sexplike) = struct
  (**
     Iterates over all ways that [query] can match the singleton list [sexp].

     For each match, calls [f], and for the duration of that call to [f], [capturebuf]
     will be filled with the resulting captures. For any index that failed to capture
     during a match, [capturebuf] will simply contain its original contents.

     Finally, restores all the original contents of [capturebuf].
  *)
  let iter_matches ~capturebuf ~f (query : Compiled_query.t) (sexp : S.t) =
    let rec iter_matches (query : Compiled_query.t) (sexps : S.t list) ~f =
      match query with
      | Capture (subquery, capture_idx) ->
        let prev_contents = capturebuf.(capture_idx) in
        (try
           (* Try catch makes sure that we properly restore [capturebuf]
              upon an exception, such as the one involved in With_return *)
           iter_matches subquery sexps ~f:(fun nconsumed rev_consumed tail ->
             capturebuf.(capture_idx) <- rev_consumed;
             f nconsumed rev_consumed tail;
             capturebuf.(capture_idx) <- prev_contents)
         with
         | exn ->
           capturebuf.(capture_idx) <- prev_contents;
           raise exn)
      | Any ->
        (match sexps with
         | [] -> ()
         | head :: tail -> f 1 [ head ] tail)
      | Atom str ->
        (match sexps with
         | [] -> ()
         | head :: tail ->
           (match S.unwrap head with
            | S.Atom a
              when a = str -> f 1 [ head ] tail
            | _ -> ()))
      | Atom_regex regex ->
        (match sexps with
         | [] -> ()
         | head :: tail ->
           (match S.unwrap head with
            | S.Atom a
              when Re2.matches regex a -> f 1 [ head ] tail
            | _ -> ()))
      | Sequence subqueries ->
        let rec loop_subsexps subqueries nconsumed rev_consumed sexps ~f =
          match subqueries with
          | [] -> f nconsumed rev_consumed sexps
          | subquery :: subqueries ->
            iter_matches subquery sexps ~f:(fun n rc tail ->
              loop_subsexps subqueries (nconsumed + n) (rc @ rev_consumed) tail ~f)
        in
        loop_subsexps subqueries 0 [] sexps ~f
      | Star subquery ->
        let rec loop nconsumed rev_consumed sexps =
          (* First try consuming zero things *)
          f nconsumed rev_consumed sexps;
          iter_matches subquery sexps ~f:(fun n rc tail ->
            (* If it consumed zero things, then we're done, break and fail since we
               already tried consuming zero ourselves. Avoids infinite loops *)
            if n = 0 then () else loop (nconsumed + n) (rc @ rev_consumed) tail)
        in
        loop 0 [] sexps
      (* Note: Q.Plus x is NOT equivalent to Q.Sequence [ x; Q.Star x ] because if x
         contains an unnamed capture, in the former x will only get assigned one capture
         index, in the latter the two copies of that unnamed capture will get different
         assigned indices *)
      | Plus subquery ->
        let rec loop nconsumed rev_consumed sexps =
          iter_matches subquery sexps ~f:(fun n rc tail ->
            let nconsumed = nconsumed + n in
            let rev_consumed = rc @ rev_consumed in
            f nconsumed rev_consumed tail;
            loop nconsumed rev_consumed tail)
        in
        loop 0 [] sexps
      | Maybe subquery ->
        (* First try consuming zero things *)
        f 0 [] sexps;
        iter_matches subquery sexps ~f:(fun n rc tail ->
          (* If it consumed zero things, then we're done, break and fail since we
             already tried consuming zero ourselves. *)
          if n = 0 then () else f n rc tail)
      | List subquery ->
        (match sexps with
         | [] -> ()
         | head :: tail ->
           (match S.unwrap head with
            | S.Atom _ -> ()
            | S.List subsexps ->
              iter_matches subquery subsexps ~f:(fun _nconsumed _rev_consumed subtail ->
                if List.is_empty subtail then f 1 [ head ] tail)))
      | Set subqueries ->
        (match sexps with
         | [] -> ()
         | head :: tail ->
           (match S.unwrap head with
            | S.Atom _ -> ()
            | S.List subsexps ->
              let rec loop_subqueries subqueries subsexps ~f =
                match subqueries with
                | [] -> f 1 [ head ] tail
                | subquery :: subquery_tail ->
                  List.iter subsexps ~f:(fun subsexp ->
                    iter_matches
                      subquery
                      [ subsexp ]
                      ~f:(fun _nconsumed _rev_consumed _subtail ->
                        loop_subqueries subquery_tail subsexps ~f))
              in
              loop_subqueries subqueries subsexps ~f))
      | Subsearch subquery ->
        let rec try_consuming_extra nconsumed rev_consumed tail ~f =
          match tail with
          | [] -> f nconsumed rev_consumed tail
          | head :: subtail ->
            try_consuming_extra (nconsumed + 1) (head :: rev_consumed) subtail ~f;
            f nconsumed rev_consumed tail
        in
        let rec loop_sexps subquery sexps nconsumed rev_consumed ~f =
          (* Try this node *)
          iter_matches subquery sexps ~f:(fun n rc tail ->
            f (nconsumed + n) (rc @ rev_consumed) tail);
          (* Then, go deep *)
          (match sexps with
           | [] -> ()
           | head :: tail ->
             (match S.unwrap head with
              | S.Atom _ -> ()
              | S.List subsexps ->
                loop_sexps subquery subsexps 0 [] ~f:(fun _subn _subrc _subtail ->
                  f (nconsumed + 1) (head :: rev_consumed) tail)));
          (* Then, go wide *)
          match sexps with
          | [] -> ()
          | head :: tail ->
            loop_sexps subquery tail (nconsumed + 1) (head :: rev_consumed) ~f
        in
        loop_sexps subquery sexps 0 [] ~f:(fun nconsumed rev_consumed tail ->
          try_consuming_extra nconsumed rev_consumed tail ~f)
      | And subqueries ->
        let rec loop_subqueries subqueries ~required_nconsumed ~rev_consumed ~tail ~f =
          match subqueries with
          | [] -> f required_nconsumed rev_consumed tail
          | subquery :: subqueries ->
            iter_matches subquery sexps ~f:(fun nconsumed _rc tl ->
              if nconsumed = required_nconsumed
              then (
                assert (phys_equal tail tl);
                loop_subqueries subqueries ~required_nconsumed ~rev_consumed ~tail ~f))
        in
        (match subqueries with
         | [] -> f 0 [] sexps
         | subquery :: subqueries ->
           iter_matches subquery sexps ~f:(fun nconsumed rev_consumed tail ->
             loop_subqueries
               subqueries
               ~required_nconsumed:nconsumed
               ~rev_consumed
               ~tail
               ~f))
      | Or_shortcircuiting subqueries ->
        let rec loop subqueries ~f =
          match subqueries with
          | [] -> ()
          | subquery :: subqueries ->
            let found_any = ref false in
            iter_matches subquery sexps ~f:(fun nconsumed rev_consumed tail ->
              found_any := true;
              f nconsumed rev_consumed tail);
            if not !found_any then loop subqueries ~f
        in
        loop subqueries ~f
      | Or_all subqueries ->
        List.iter subqueries ~f:(fun subquery -> iter_matches subquery sexps ~f)
      | First_match_only subquery ->
        With_return.with_return (fun { return } ->
          iter_matches subquery sexps ~f:(fun nconsumed rev_consumed tail ->
            f nconsumed rev_consumed tail;
            return ()))
    in
    iter_matches query [ sexp ] ~f:(fun _nconsumed _rev_consumed tail ->
      if List.is_empty tail then f ())
  ;;
end

module Sexp_engine = Make_engine (Sexp)
module Labeled_sexp_engine = Make_engine (Labeled_sexp)

let combine_results results ~wrap_singletons : Sexp.t =
  match results with
  | [] -> List []
  | [ x ] -> if wrap_singletons then List [ x ] else x
  | _ -> List (List.rev results)
;;

let iter_matches ~query ~output ~wrap_singletons sexp ~f =
  let query, `Names_of_captures names_of_captures, output_type =
    Compiled_query.create query output
  in
  let capturebuf = Array.map names_of_captures ~f:(fun _ -> []) in
  match output_type with
  | `Output_single_capture ->
    Sexp_engine.iter_matches ~capturebuf query sexp ~f:(fun () ->
      f (combine_results capturebuf.(0) ~wrap_singletons))
  | `Output_as_list ->
    Sexp_engine.iter_matches ~capturebuf query sexp ~f:(fun () ->
      f
        (Sexp.List
           (Array.to_list (Array.map capturebuf ~f:(combine_results ~wrap_singletons)))))
  | `Output_as_record ->
    Sexp_engine.iter_matches ~capturebuf query sexp ~f:(fun () ->
      f
        (Sexp.List
           (Array.map2_exn
              names_of_captures
              capturebuf
              ~f:(fun capture_name captured ->
                Sexp.List
                  [ Sexp.Atom capture_name; combine_results captured ~wrap_singletons ])
            |> Array.to_list)))
  | `Output output ->
    let capture_name_to_idx_map =
      Array.mapi names_of_captures ~f:(fun i capture_name -> capture_name, i)
      |> Array.to_list
      |> String.Table.of_alist_exn
    in
    Sexp_engine.iter_matches ~capturebuf query sexp ~f:(fun () ->
      f
        (Output.embed_captures output ~f:(fun capture_name ->
           combine_results
             ~wrap_singletons
             capturebuf.(Hashtbl.find_exn capture_name_to_idx_map capture_name))))
;;

let replace ~query ~replace ~with_:output ~wrap_singletons sexp =
  let query, `Names_of_captures names_of_captures, _output_type =
    Compiled_query.create query (Some output)
  in
  let replace_capture_name =
    if String.is_prefix replace ~prefix:"%"
    then String.chop_prefix_exn replace ~prefix:"%"
    else failwithf "Replacement target '%s' does not start with '%%'" replace ()
  in
  (* Checked in [Compiled_query.create], assert again here *)
  assert (Array.mem names_of_captures replace_capture_name ~equal:String.equal);
  let capturebuf = Array.map names_of_captures ~f:(fun _ -> []) in
  let sexp = Labeled_sexp.of_sexp sexp in
  let capture_name_to_idx_map =
    Array.mapi names_of_captures ~f:(fun i capture_name -> capture_name, i)
    |> Array.to_list
    |> String.Table.of_alist_exn
  in
  let replace_idx = Hashtbl.find_exn capture_name_to_idx_map replace_capture_name in
  (* Two-pass algorithm, first iterate over all matches and record the labeled sexp id
     of the subsexp that the [replace] target hit along with the captures at that time. *)
  let planned_replacements = Int.Table.create () in
  Labeled_sexp_engine.iter_matches ~capturebuf query sexp ~f:(fun () ->
    match capturebuf.(replace_idx) with
    | [] -> ()
    | _ :: _ :: _ ->
      failwithf
        "Replacement target %s captured more than one sexp, currently\n\
        \          replace only supports patterns where the replacement target \
         captures a single sexp"
        replace
        ()
    | [ replacement_target ] ->
      let replacement_target_id = replacement_target.node_id in
      let result =
        Output.embed_captures output ~f:(fun capture_name ->
          let capture =
            List.map
              capturebuf.(Hashtbl.find_exn capture_name_to_idx_map capture_name)
              ~f:Labeled_sexp.to_sexp
          in
          combine_results ~wrap_singletons capture)
      in
      (* If a particular sexp gets hit for replacement more than once, arbitrarily just
         choose the first possible one *)
      if not (Hashtbl.mem planned_replacements replacement_target_id)
      then Hashtbl.set planned_replacements ~key:replacement_target_id ~data:result);
  (* Second pass, perform all replacements *)
  Labeled_sexp.to_sexp_with_replacements sexp ~f:(fun id ->
    Hashtbl.find planned_replacements id)
;;

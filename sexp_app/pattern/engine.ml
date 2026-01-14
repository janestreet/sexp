open Core

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

  let rec to_sexp_with_replacements t ~f : Sexp.t list =
    match f t.node_id with
    | `Replace replacements -> replacements
    | `Preserve_and_recurse ->
      (match t.unwrapped with
       | Atom s -> [ Sexp.Atom s ]
       | List list ->
         [ Sexp.List (List.concat_map list ~f:(to_sexp_with_replacements ~f)) ])
  ;;
end

module Make_engine (S : Sexplike) = struct
  (** Iterates over all ways that [query] can match the singleton list [sexp].

      For each match, calls [f], and for the duration of that call to [f],
      [revcapture_buf] will be filled with the resulting captures, where each entry of
      [revcapture_buf] contains the captured sequence for that index in reverse order. For
      any index that failed to capture during a match, [revcapture_buf] will simply
      contain its original contents.

      Finally, restores all the original contents of [revcapture_buf]. *)
  let iter_matches ~revcapture_buf ~f (query : Compiled_query.t) (sexp : S.t) =
    let rec iter_matches (query : Compiled_query.t) (sexps : S.t list) ~f =
      match query with
      | Capture (subquery, capture_idx) ->
        let prev_contents = revcapture_buf.(capture_idx) in
        (* Try catch makes sure that we properly restore [revcapture_buf] upon an
           exception, such as the one involved in With_return *)
        (try
           iter_matches subquery sexps ~f:(fun nconsumed rev_consumed tail ->
             revcapture_buf.(capture_idx) <- rev_consumed;
             f nconsumed rev_consumed tail;
             revcapture_buf.(capture_idx) <- prev_contents)
         with
         | exn ->
           revcapture_buf.(capture_idx) <- prev_contents;
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
            | S.Atom a when String.( = ) a str -> f 1 [ head ] tail
            | _ -> ()))
      | Atom_regex regex ->
        (match sexps with
         | [] -> ()
         | head :: tail ->
           (match S.unwrap head with
            | S.Atom a when Re2.matches regex a -> f 1 [ head ] tail
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
        (* [Star q] is equivalent to [Or_all [Sequence []; Sequence [q; Star q]]] (modulo
           numbering of unlabeled captures), *)
        let rec loop nconsumed rev_consumed sexps =
          (* so first try consuming zero things *)
          f nconsumed rev_consumed sexps;
          (* then try consuming q followed by [Star q]. *)
          iter_matches subquery sexps ~f:(fun n rc tail ->
            (* If it consumed zero things, then we're done, break and fail since we
               already tried consuming zero ourselves. Avoids infinite loops *)
            if n = 0 then () else loop (nconsumed + n) (rc @ rev_consumed) tail)
        in
        loop 0 [] sexps
      | Star_greedy subquery ->
        (* [Star_greedy q] is equivalent to
           [Or_all [Sequence [q; Star_greedy q]; Sequence []]] (modulo numbering of
           unlabeled captures), *)
        let rec loop nconsumed rev_consumed sexps =
          (* so first try consuming q followed by [Star_greedy q] *)
          iter_matches subquery sexps ~f:(fun n rc tail ->
            (* If it consumed zero things, skip it since we will try consuming zero
               ourselves. Avoids infinite loops *)
            if n = 0 then () else loop (nconsumed + n) (rc @ rev_consumed) tail);
          (* then, try consuming zero things *)
          f nconsumed rev_consumed sexps
        in
        loop 0 [] sexps
      | Plus subquery ->
        let rec loop nconsumed rev_consumed sexps =
          iter_matches subquery sexps ~f:(fun n rc tail ->
            let nconsumed = nconsumed + n in
            let rev_consumed = rc @ rev_consumed in
            f nconsumed rev_consumed tail;
            (* If it consumed zero things, then we're done. Avoids infinite loops. *)
            if n = 0 then () else loop nconsumed rev_consumed tail)
        in
        loop 0 [] sexps
      | Plus_greedy subquery ->
        let rec loop nconsumed rev_consumed sexps =
          iter_matches subquery sexps ~f:(fun n rc tail ->
            let nconsumed = nconsumed + n in
            let rev_consumed = rc @ rev_consumed in
            if n = 0 then () else loop nconsumed rev_consumed tail;
            f nconsumed rev_consumed tail)
        in
        loop 0 [] sexps
      | Maybe subquery ->
        (* First try consuming zero things *)
        f 0 [] sexps;
        iter_matches subquery sexps ~f:(fun n rc tail ->
          (* If it consumed zero things, then we're done, break and fail since we already
             tried consuming zero ourselves. *)
          if n = 0 then () else f n rc tail)
      | Maybe_greedy subquery ->
        iter_matches subquery sexps ~f:(fun n rc tail ->
          (* If it consumed zero things, skip it since we will try consuming zero
             ourselves. Avoids duplicate null matches. *)
          if n = 0 then () else f n rc tail);
        (* Lastly, try consuming zero things *)
        f 0 [] sexps
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
                | (subquery, { Query.Set_kind.first_only; optional }) :: subquery_tail ->
                  With_return.with_return (fun { return = stop_trying_to_find_a_match } ->
                    let found_a_match = ref false in
                    List.iter subsexps ~f:(fun subsexp ->
                      iter_matches
                        subquery
                        [ subsexp ]
                        ~f:(fun nconsumed _rev_consumed _subtail ->
                          (* If the subquery didn't actually consume the subsexp, it
                             doesn't count *)
                          if nconsumed = 0
                          then ()
                          else (
                            found_a_match := true;
                            loop_subqueries subquery_tail subsexps ~f));
                      if first_only && !found_a_match then stop_trying_to_find_a_match ());
                    if optional && not !found_a_match
                    then loop_subqueries subquery_tail subsexps ~f)
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

let maybe_wrap_results
  (type query_result)
  ~(revcapture : Sexp.t list)
  ~(wrap_mode : query_result Output_method.Wrap_mode.t)
  : query_result
  =
  match wrap_mode with
  | Unwrap_always -> List.rev revcapture
  | Wrap_non_singletons ->
    (match revcapture with
     | [] -> List []
     | [ x ] -> x
     | _ -> List (List.rev revcapture))
  | Wrap_always -> List (List.rev revcapture)
;;

let iter_matches
  (type a)
  ~query
  ~(output_method : a Output_method.t)
  sexp
  ~(f : a -> unit)
  =
  let query, `Labels_of_captures labels_of_captures =
    Compiled_query.create query output_method
  in
  let revcapture_buf = Array.map labels_of_captures ~f:(fun _ -> []) in
  match output_method with
  | Single_capture wrap_mode ->
    Sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
      f (maybe_wrap_results ~revcapture:revcapture_buf.(0) ~wrap_mode))
  | List wrap_mode ->
    Sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
      let results =
        List.map (Array.to_list revcapture_buf) ~f:(fun revcapture ->
          maybe_wrap_results ~revcapture ~wrap_mode)
      in
      let results =
        match wrap_mode with
        | Unwrap_always -> List.concat results
        | Wrap_non_singletons -> results
        | Wrap_always -> results
      in
      f (Sexp.List results))
  | Record wrap_mode ->
    Sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
      let results =
        Array.map2_exn
          labels_of_captures
          revcapture_buf
          ~f:(fun capture_label revcapture ->
            let field_name = Sexp.Atom capture_label in
            let capture_result = maybe_wrap_results ~revcapture ~wrap_mode in
            match wrap_mode with
            | Unwrap_always -> Sexp.List (field_name :: capture_result)
            | Wrap_non_singletons -> Sexp.List [ field_name; capture_result ]
            | Wrap_always -> Sexp.List [ field_name; capture_result ])
        |> Array.to_list
      in
      f (Sexp.List results))
  | Formats (wrap_mode, formats) ->
    let capture_label_to_idx_map =
      Array.mapi labels_of_captures ~f:(fun i capture_label -> capture_label, i)
      |> Array.to_list
      |> String.Table.of_alist_exn
    in
    Sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
      let format_results =
        List.concat_map formats ~f:(fun format ->
          Output_method.Format.embed_captures format ~f:(fun capture_label ->
            let capture_idx = Hashtbl.find_exn capture_label_to_idx_map capture_label in
            let capture_result =
              maybe_wrap_results ~wrap_mode ~revcapture:revcapture_buf.(capture_idx)
            in
            match wrap_mode with
            | Unwrap_always -> capture_result
            | Wrap_non_singletons -> [ capture_result ]
            | Wrap_always -> [ capture_result ]))
      in
      f format_results)
  | Map ->
    Sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
      f
        (Array.map2_exn labels_of_captures revcapture_buf ~f:(fun label revcaptures ->
           label, List.rev revcaptures)
         |> Array.to_list
         |> String.Map.of_alist_exn))
;;

let no_planned_replacements_yet ~planned_replacements ~targets =
  List.for_all targets ~f:(fun target ->
    not (Hashtbl.mem planned_replacements target.Labeled_sexp.node_id))
;;

(* [planned_replcements] is a map from id to the list of sexps to splice

   To replace the sequence [foo bar baz] with [whizz wang], we set planned_replacements to
   splice in [whizz wang] where [foo] is and to remove [bar] and [baz]. Before calling
   this function we check that none of the targets are due to be replaced using
   [no_planned_replacements_yet]. *)
let replace_sequence_with ~planned_replacements ~targets ~desired =
  assert (not (List.is_empty targets));
  (* Replace the sequence of targets with the desired results sequence. Do this by first
     replacing the first sexp with the desired sequence... *)
  Hashtbl.set
    planned_replacements
    ~key:(List.hd_exn targets).Labeled_sexp.node_id
    ~data:desired;
  (* ... then deleting all the rest *)
  List.iter (List.tl_exn targets) ~f:(fun target ->
    Hashtbl.set planned_replacements ~key:target.node_id ~data:[])
;;

let replace
  (type a)
  ~query
  ~replace
  ~with_:formats
  ~(wrap_mode : a Output_method.Wrap_mode.t)
  sexp
  =
  let query, `Labels_of_captures labels_of_captures =
    Compiled_query.create query (Formats (wrap_mode, formats))
  in
  let replace_capture_label =
    if String.is_prefix replace ~prefix:"%"
    then String.chop_prefix_exn replace ~prefix:"%"
    else failwithf "Replacement target '%s' does not start with '%%'" replace ()
  in
  if not (Array.mem labels_of_captures replace_capture_label ~equal:String.equal)
  then
    failwithf
      "Attempting to replace %%%s but it does not occur in the query pattern"
      replace_capture_label
      ();
  let revcapture_buf = Array.map labels_of_captures ~f:(fun _ -> []) in
  let sexp = Labeled_sexp.of_sexp sexp in
  let capture_label_to_idx_map =
    Array.mapi labels_of_captures ~f:(fun i capture_label -> capture_label, i)
    |> Array.to_list
    |> String.Table.of_alist_exn
  in
  let replace_idx = Hashtbl.find_exn capture_label_to_idx_map replace_capture_label in
  (* Two-pass algorithm, first iterate over all matches and record the labeled sexp id of
     the subsexp that the [replace] target hit along with the captures at that time. *)
  let planned_replacements = Int.Table.create () in
  Labeled_sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
    match List.rev revcapture_buf.(replace_idx) with
    | [] -> ()
    | replacement_targets ->
      (* Whenever a later replacement would overlap with a prior one, do nothing instead. *)
      if no_planned_replacements_yet ~planned_replacements ~targets:replacement_targets
      then (
        let replacements =
          List.concat_map formats ~f:(fun format ->
            Output_method.Format.embed_captures format ~f:(fun capture_label ->
              let capture_idx = Hashtbl.find_exn capture_label_to_idx_map capture_label in
              let revcapture =
                List.map revcapture_buf.(capture_idx) ~f:Labeled_sexp.to_sexp
              in
              let capture_result = maybe_wrap_results ~wrap_mode ~revcapture in
              match wrap_mode with
              | Unwrap_always -> capture_result
              | Wrap_non_singletons -> [ capture_result ]
              | Wrap_always -> [ capture_result ]))
        in
        replace_sequence_with
          ~planned_replacements
          ~targets:replacement_targets
          ~desired:replacements));
  (* Second pass, perform all replacements *)
  Labeled_sexp.to_sexp_with_replacements sexp ~f:(fun id ->
    match Hashtbl.find planned_replacements id with
    | None -> `Preserve_and_recurse
    | Some replacements -> `Replace replacements)
;;

let replace' ~query ~f sexp =
  let query, `Labels_of_captures labels_of_captures = Compiled_query.create query Map in
  let sexp = Labeled_sexp.of_sexp sexp in
  let revcapture_buf = Array.map labels_of_captures ~f:(fun _ -> []) in
  let capture_label_to_idx_map =
    Array.mapi labels_of_captures ~f:(fun i capture_label -> capture_label, i)
    |> Array.to_list
    |> String.Map.of_alist_exn
  in
  (* Two-pass algorithm, first iterate over all matches and record the labeled sexp id of
     the subsexp that the [replace] target hit along with the captures at that time. *)
  let planned_replacements = Int.Table.create () in
  Labeled_sexp_engine.iter_matches ~revcapture_buf query sexp ~f:(fun () ->
    let captures_by_label =
      String.Map.map capture_label_to_idx_map ~f:(fun idx ->
        let revcapture = revcapture_buf.(idx) in
        List.rev_map revcapture ~f:Labeled_sexp.to_sexp)
    in
    let replacements = f captures_by_label in
    let replacement_targets_and_replacements =
      Map.to_alist (replacements : _ String.Map.t)
      |> List.map ~f:(fun (label, replacements) ->
        match Map.find capture_label_to_idx_map label with
        | None ->
          failwithf
            "In [Pattern.Engine.replace'], [f] returned a map of replacements that \
             contains a key that is not the label of a capture: %s"
            label
            ()
        | Some idx ->
          let targets = List.rev revcapture_buf.(idx) in
          if List.is_empty targets && not (List.is_empty replacements)
          then
            failwithf
              "In [Pattern.Engine.replace'], [f] returned a map of replacements that \
               contains a key that for a label with zero captures: %s"
              label
              ();
          targets, replacements)
    in
    (* Whenever a later replacement would overlap with a prior one, do nothing instead. *)
    if List.for_all replacement_targets_and_replacements ~f:(fun (targets, _) ->
         no_planned_replacements_yet ~planned_replacements ~targets)
    then
      List.iter replacement_targets_and_replacements ~f:(fun (targets, replacements) ->
        if not (List.is_empty targets)
        then replace_sequence_with ~planned_replacements ~targets ~desired:replacements));
  (* Second pass, perform all replacements *)
  Labeled_sexp.to_sexp_with_replacements sexp ~f:(fun id ->
    match Hashtbl.find planned_replacements id with
    | None -> `Preserve_and_recurse
    | Some replacements -> `Replace replacements)
;;

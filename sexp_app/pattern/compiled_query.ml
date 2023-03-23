open Core

type t =
  | Capture of t * int
  | Any
  | Atom of string
  | Atom_regex of Re2.t
  | Sequence of t list
  | Star of t
  | Star_greedy of t
  | Plus of t
  | Plus_greedy of t
  | Maybe of t
  | Maybe_greedy of t
  | List of t
  | Set of (t * Query.Set_kind.t) list
  | Subsearch of t
  | And of t list
  | Or_shortcircuiting of t list
  | Or_all of t list
  | First_match_only of t
[@@deriving sexp_of]

let of_query query ~idx_of_unlabeled_capture ~idx_of_number_capture ~idx_of_named_capture =
  let rec compile_list = List.map ~f:compile
  and compile_set_arg = List.map ~f:(fun (query, mode) -> compile query, mode)
  and compile query : t =
    match (query : Query.t) with
    (* capture lookup *)
    | Capture_unlabeled sub -> Capture (compile sub, idx_of_unlabeled_capture ())
    | Capture_to_number (i, sub) -> Capture (compile sub, idx_of_number_capture i)
    | Capture_to_name (name, sub) -> Capture (compile sub, idx_of_named_capture name)
    (* regex compilation *)
    | Atom_regex s -> Atom_regex (Re2.create_exn s)
    (* traversal *)
    | Any -> Any
    | Atom s -> Atom s
    | Sequence subs -> Sequence (compile_list subs)
    | Star sub -> Star (compile sub)
    | Star_greedy sub -> Star_greedy (compile sub)
    | Plus sub -> Plus (compile sub)
    | Plus_greedy sub -> Plus_greedy (compile sub)
    | Maybe sub -> Maybe (compile sub)
    | Maybe_greedy sub -> Maybe_greedy (compile sub)
    | List sub -> List (compile sub)
    | Set subs -> Set (compile_set_arg subs)
    | Subsearch sub -> Subsearch (compile sub)
    | And subs -> And (compile_list subs)
    | Or_shortcircuiting subs -> Or_shortcircuiting (compile_list subs)
    | Or_all subs -> Or_all (compile_list subs)
    | First_match_only sub -> First_match_only (compile sub)
  in
  compile query
;;

let create (type a) (uncompiled_query : Query.t) (output_method : a Output_method.t) =
  Query.validate_all_captures_labeled_or_all_unlabeled_exn uncompiled_query;
  let number_captures = Queue.create () in
  let named_captures = Queue.create () in
  let num_unlabeled_captures = ref 0 in
  Query.iter uncompiled_query ~f:(function
    | Capture_unlabeled _ -> incr num_unlabeled_captures
    | Capture_to_number (number, _) -> Queue.enqueue number_captures number
    | Capture_to_name (name, _) -> Queue.enqueue named_captures name
    | _ -> ());
  let fail_unsupported_capture ~kind =
    raise_s
      (let query_pattern = uncompiled_query in
       [%message
         (sprintf
            "Query pattern contains %s capture, but they are not allowed when using this \
             output method"
            kind)
           (query_pattern : Query.t)
           (output_method : _ Output_method.t)])
  in
  let fail_unlabeled_capture () = fail_unsupported_capture ~kind:"unlabeled" in
  let fail_numbered_capture () = fail_unsupported_capture ~kind:"numbered" in
  let fail_named_capture () = fail_unsupported_capture ~kind:"named" in
  let pick_indices_for_named_and_number_captures () =
    let last_idx = ref (-1) in
    let get_idx () =
      incr last_idx;
      !last_idx
    in
    let idx_of_label = String.Table.create () in
    let compiled_query =
      of_query
        uncompiled_query
        ~idx_of_unlabeled_capture:(fun () -> fail_unlabeled_capture ())
        ~idx_of_number_capture:(fun n ->
          Hashtbl.find_or_add idx_of_label (Int.to_string n) ~default:get_idx)
        ~idx_of_named_capture:(fun name ->
          Hashtbl.find_or_add idx_of_label name ~default:get_idx)
    in
    let label_of_idx =
      Hashtbl.to_alist idx_of_label
      |> List.map ~f:(fun (label, idx) -> idx, label)
      |> List.sort ~compare:(fun (idx0, _) (idx1, _) -> Int.compare idx0 idx1)
    in
    List.iteri label_of_idx ~f:(fun i (idx, _) -> assert (i = idx));
    compiled_query, Array.of_list (List.map label_of_idx ~f:snd)
  in
  let compiled_query, labels_of_captures =
    match output_method with
    | Formats (_, formats) ->
      let used_labels = String.Hash_set.create () in
      Queue.iter named_captures ~f:(fun name -> Hash_set.add used_labels name);
      Queue.iter number_captures ~f:(fun number ->
        Hash_set.add used_labels (Int.to_string number));
      List.iter formats ~f:(fun format ->
        List.iter (Output_method.Format.all_captures format) ~f:(fun c ->
          if not (Hash_set.mem used_labels c)
          then
            failwithf
              "Output or replacement expression uses capture not present in pattern: %s"
              c
              ()));
      pick_indices_for_named_and_number_captures ()
    | Record _ -> pick_indices_for_named_and_number_captures ()
    | Map -> pick_indices_for_named_and_number_captures ()
    | List _ ->
      if Queue.length number_captures > 0
      then (
        let used_idxs = Int.Hash_set.create () in
        Queue.iter number_captures ~f:(fun number ->
          if number < 0
          then
            raise_s
              [%message
                "Attempted to use negative number as index for a numbered capture"
                  (number : int)];
          Hash_set.add used_idxs number);
        let max_used_idx = Hash_set.to_list used_idxs |> List.reduce_exn ~f:Int.max in
        for i = 0 to max_used_idx do
          if not (Hash_set.mem used_idxs i)
          then
            if i = 0
            then
              failwithf
                "Match pattern uses captures up to %%%d but is missing %%%d (reminder: \
                 numbered captures should be zero-indexed)"
                max_used_idx
                i
                ()
            else
              failwithf
                "Match pattern uses captures up to %%%d but is missing %%%d"
                max_used_idx
                i
                ()
        done;
        let compiled_query =
          of_query
            uncompiled_query
            ~idx_of_unlabeled_capture:(fun () -> fail_unlabeled_capture ())
            ~idx_of_number_capture:(fun n -> n)
            ~idx_of_named_capture:(fun _ -> fail_named_capture ())
        in
        compiled_query, Array.init (max_used_idx + 1) ~f:Int.to_string)
      else (
        (* Assign incrementing integer index by default for unlabeled captures *)
        let num_used_idxs = ref 0 in
        let compiled_query =
          of_query
            uncompiled_query
            ~idx_of_unlabeled_capture:(fun () ->
              let idx = !num_used_idxs in
              incr num_used_idxs;
              idx)
            ~idx_of_number_capture:(fun _ -> fail_numbered_capture ())
            ~idx_of_named_capture:(fun _ -> fail_named_capture ())
        in
        compiled_query, Array.init !num_used_idxs ~f:Int.to_string)
    | Single_capture _ ->
      if !num_unlabeled_captures <> 1
      then
        raise_s
          (let query_pattern = uncompiled_query in
           [%message
             "Query pattern has 0 or multiple unlabeled captures, which is not allowed \
              if using this output method"
               (query_pattern : Query.t)
               (output_method : _ Output_method.t)]);
      let compiled_query =
        of_query
          uncompiled_query
          ~idx_of_unlabeled_capture:(fun () -> 0)
          ~idx_of_number_capture:(fun _ -> fail_numbered_capture ())
          ~idx_of_named_capture:(fun _ -> fail_named_capture ())
      in
      compiled_query, [| "0" |]
  in
  compiled_query, `Labels_of_captures labels_of_captures
;;

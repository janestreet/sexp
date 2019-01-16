open Core

type t =
  | Capture of t * int
  | Any
  | Atom of string
  | Atom_regex of Re2.t
  | Sequence of t list
  | Star of t
  | Plus of t
  | Maybe of t
  | List of t
  | Set of t list
  | Subsearch of t
  | And of t list
  | Or_shortcircuiting of t list
  | Or_all of t list
  | First_match_only of t
[@@deriving sexp_of]

let of_query query ~idx_of_unlabeled_capture ~idx_of_number_capture ~idx_of_field_capture
  =
  let rec compile_list = List.map ~f:compile
  and compile query : t =
    match (query : Query.t) with
    (* capture lookup *)
    | Capture_unlabeled sub -> Capture (compile sub, idx_of_unlabeled_capture ())
    | Capture_to_number (i, sub) -> Capture (compile sub, idx_of_number_capture i)
    | Capture_to_field (field, sub) -> Capture (compile sub, idx_of_field_capture field)
    (* regex compilation *)
    | Atom_regex s -> Atom_regex (Re2.create_exn s)
    (* traversal *)
    | Any -> Any
    | Atom s -> Atom s
    | Sequence subs -> Sequence (compile_list subs)
    | Star sub -> Star (compile sub)
    | Plus sub -> Plus (compile sub)
    | Maybe sub -> Maybe (compile sub)
    | List sub -> List (compile sub)
    | Set subs -> Set (compile_list subs)
    | Subsearch sub -> Subsearch (compile sub)
    | And subs -> And (compile_list subs)
    | Or_shortcircuiting subs -> Or_shortcircuiting (compile_list subs)
    | Or_all subs -> Or_all (compile_list subs)
    | First_match_only sub -> First_match_only (compile sub)
  in
  compile query
;;

let create (uncompiled_query : Query.t) output =
  let number_captures = Queue.create () in
  let field_captures = Queue.create () in
  let num_unlabeled_captures = ref 0 in
  Query.iter uncompiled_query ~f:(function
    | Capture_unlabeled _ -> incr num_unlabeled_captures
    | Capture_to_number (i, _) -> Queue.enqueue number_captures i
    | Capture_to_field (field, _) -> Queue.enqueue field_captures field
    | _ -> ());
  if Queue.length number_captures > 0 || Queue.length field_captures > 0
  then
    if !num_unlabeled_captures > 0
    then
      failwith
        "Cannot mix unlabeled captures with named or numbered captures in the same \
         pattern";
  let output_type =
    match output with
    | Some output -> `Output output
    | None ->
      if Queue.length field_captures > 0
      then `Output_as_record
      else if Queue.length number_captures > 0 || !num_unlabeled_captures > 1
      then `Output_as_list
      else if !num_unlabeled_captures = 1
      then `Output_single_capture
      else failwith "No captures % were specified in pattern"
  in
  let compiled_query, names_of_captures =
    match output_type with
    | (`Output_as_record | `Output _) as output_type ->
      let used_labels = String.Hash_set.create () in
      Queue.iter field_captures ~f:(fun field -> Hash_set.add used_labels field);
      Queue.iter number_captures ~f:(fun number ->
        Hash_set.add used_labels (Int.to_string number));
      let last_idx = ref (-1) in
      let get_idx () =
        incr last_idx;
        !last_idx
      in
      let idx_of_label = String.Table.create () in
      let compiled_query =
        of_query
          uncompiled_query
          ~idx_of_unlabeled_capture:(fun () -> assert false)
          ~idx_of_number_capture:(fun n ->
            Hashtbl.find_or_add idx_of_label (Int.to_string n) ~default:get_idx)
          ~idx_of_field_capture:(fun field ->
            Hashtbl.find_or_add idx_of_label field ~default:get_idx)
      in
      let label_of_idx =
        Hashtbl.to_alist idx_of_label
        |> List.map ~f:(fun (label, idx) -> idx, label)
        |> List.sort ~compare:(fun (idx0, _) (idx1, _) -> Int.compare idx0 idx1)
      in
      List.iteri label_of_idx ~f:(fun i (idx, _) -> assert (i = idx));
      (match output_type with
       | `Output_as_record -> ()
       | `Output output ->
         List.iter (Output.all_captures output) ~f:(fun c ->
           if not (Hash_set.mem used_labels c)
           then
             failwithf
               "Output or replacement expression uses capture not present in pattern: %s"
               c
               ()));
      compiled_query, Array.of_list (List.map label_of_idx ~f:snd)
    | `Output_as_list ->
      assert (Queue.is_empty field_captures);
      if Queue.length number_captures > 0
      then (
        let used_idxs = Int.Hash_set.create () in
        Queue.iter number_captures ~f:(fun number ->
          assert (number >= 0);
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
            ~idx_of_unlabeled_capture:(fun () -> assert false)
            ~idx_of_number_capture:(fun n -> n)
            ~idx_of_field_capture:(fun _ -> assert false)
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
            ~idx_of_number_capture:(fun _ -> assert false)
            ~idx_of_field_capture:(fun _ -> assert false)
        in
        compiled_query, Array.init !num_used_idxs ~f:Int.to_string)
    | `Output_single_capture ->
      assert (Queue.is_empty field_captures);
      assert (Queue.is_empty number_captures);
      assert (!num_unlabeled_captures = 1);
      let compiled_query =
        of_query
          uncompiled_query
          ~idx_of_unlabeled_capture:(fun () -> 0)
          ~idx_of_number_capture:(fun _ -> assert false)
          ~idx_of_field_capture:(fun _ -> assert false)
      in
      compiled_query, [| "0" |]
  in
  compiled_query, `Names_of_captures names_of_captures, output_type
;;

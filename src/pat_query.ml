open! Core

(*=We want to support sorting by multiple captured values, e.g., with -pat-query "{
   (field1 %0) (field2 %1) }", so we want to use the List output method. If there's only
   one capture, we'll use Single_capture. (This matches the behavior of
   default_output_method, which uses List when there are any numbered captures, or
   multiple unlabeled captures, and Single_capture when there's just a single unlabeled
   capture.) Using Single_capture when possible will avoid allocating an extra Sexp.List
   for every capture.

   For wrap_mode, the pat-query documentation explains the different options by
   showing what happens when "(a %[.*])" matches "(a)", "(a b)", and "(a b c)":

   Unwrap_always:
   (a)     -> (*  *)
   (a b)   -> (* b *)
   (a b c) -> (* b c *)

   Wrap_non_singletons:
   (a)     -> (* () *)
   (a b)   -> (* b *)
   (a b c) -> (* (b c) *)

   Wrap_always:
   (a)     -> (* () *)
   (a b)   -> (* (b) *)
   (a b c) -> (* (b c) *)

   We definitely don't want Wrap_non_singletons, becuase that would cause "(a b)"
   (extracted key = "b") to be sorted before "(a)" (extracted key = "()"), but I'm not
   sure if there's a difference between Unwrap_always and Wrap_always.

   Using Unwrap_always can cause different inputs to produce the same keys in a way that
   is probably not what the user intended:

   $ echo "((a 1)(b 5 3 4))   ((a 1 2)(b 3 4))" > wrap.sexp
   $ cat wrap.sexp | sexp pat-query "{ (a %0=[.*]) (b %1=[.*]) }" # Wrap_non_singletons
   (1 (5 3 4))
   ((1 2) (3 4))
   $ cat wrap.sexp | sexp pat-query "{ (a %0=[.*]) (b %1=[.*]) }" -wrap # Wrap_always
   ((1) (5 3 4))
   ((1 2) (3 4))
   $ cat wrap.sexp | sexp pat-query "{ (a %0=[.*]) (b %1=[.*]) }" -unwrap # Unwrap_always
   (1 5 3 4)
   (1 2 3 4)

   If we were to sort the input sexps by the output of the pat-query, we would get
   different results depending on if we used Wrap_always or Unwrap_always. Looking at
   the pattern, very likely we want to sort by the tuple "(<everything after a>,
   <everything after b>)", which corresponds to the behavior of Wrap_always.
*)

let single_capture_output_method : Sexp.t Sexp_app_pattern.Output_method.t =
  Single_capture Wrap_always
;;

let multi_or_numbered_capture_output_method : Sexp.t Sexp_app_pattern.Output_method.t =
  List Wrap_always
;;

let run (query : Sexp_app_pattern.Query.t) : (Sexp.t -> Sexp.t list) Staged.t =
  let ({ num_number_captures; num_unlabeled_captures; _ }
        : Sexp_app_pattern.Query.Capture_count.t)
    =
    Sexp_app_pattern.Query.count_captures query
  in
  let output_method =
    if num_number_captures = 0 && num_unlabeled_captures = 1
    then single_capture_output_method
    else multi_or_numbered_capture_output_method
  in
  stage (fun sexp ->
    let keys = ref [] in
    Sexp_app_pattern.Engine.iter_matches ~query ~output_method sexp ~f:(fun sexp ->
      keys := sexp :: !keys);
    List.rev !keys)
;;

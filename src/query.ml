open Core
open Sexp_app

type output_mode =
  | Sexp
  | Count
  | Silent

type t =
  { inputs : unit Located.t
  ; output_mode : output_mode
  ; allow_empty_output : bool
  ; labeled : bool
  ; group : bool
  ; machine : bool
  ; fail_on_parse_error : bool
  ; perform_query : Sexp_ext.t -> on_result:(Sexp.t -> unit) -> unit
  }

type parameters = t

let singleton x = Lazy_list.cons x (Lazy_list.empty ())

let scan lexbuf ~fail_on_parse_error =
  Lazy_list.build ~seed:() ~f:(fun () ->
    try
      match Sexp.scan_sexp_opt lexbuf with
      | None -> None
      | Some sexp -> Some (Sexp_ext.t_of_sexp sexp, ())
    with
    | _ignored_exn when not fail_on_parse_error -> None)
;;

module Transform : sig
  type t

  val make : parameters -> t
  val initialize_source : t -> string -> Sexp.t -> unit
  val finalize_source : t -> string -> unit
  val finalize_all : t -> unit
  val any_output : t -> bool
end = struct
  type t =
    { initialize_source : string -> Sexp.t -> unit
    ; process_sexp : Sexp.t -> unit
    ; finalize_source : string -> unit
    ; finalize_all : unit -> unit
    ; any_output : bool ref
    }

  let initialize_source t label = t.initialize_source label
  let finalize_source t label = t.finalize_source label
  let finalize_all t = t.finalize_all ()
  let any_output t = t.any_output.contents
  let with_label label sexp = Sexp.List [ Sexp.Atom label; sexp ]

  let make_count t ~f =
    let count = ref 0 in
    let process_sexp _sexp = incr count in
    if t.labeled
    then (
      let finalize_source label =
        let sexp = with_label label (Int.sexp_of_t !count) in
        count := 0;
        f sexp
      in
      let finalize_all = ignore in
      process_sexp, finalize_source, finalize_all)
    else (
      let finalize_source = ignore in
      let finalize_all () =
        let count = Int.sexp_of_t !count in
        f count
      in
      process_sexp, finalize_source, finalize_all)
  ;;

  let make t =
    let any_output = ref t.allow_empty_output in
    let sexp_output = if t.machine then Sexp.output else Sexp.output_hum in
    let process_sexp sexp =
      sexp_output stdout sexp;
      print_endline ""
    in
    let process_output ~f =
      if t.allow_empty_output
      then f
      else
        fun sexp ->
        f sexp;
        any_output := true
    in
    let process_sexp, finalize_source, finalize_all =
      match t.output_mode with
      | Sexp -> process_output ~f:process_sexp, ignore, ignore
      | Count -> make_count t ~f:(process_output ~f:process_sexp)
      | Silent -> process_output ~f:ignore, ignore, ignore
    in
    let initialize_source =
      if t.labeled
      then (fun label ->
        ();
        fun sexp -> process_sexp (with_label label sexp))
      else fun _ -> process_sexp
    in
    { initialize_source; process_sexp; finalize_source; finalize_all; any_output }
  ;;
end

let execute t =
  let transform = Transform.make t in
  let channels = Located.channels t.inputs in
  let input =
    Located.map channels ~f:(fun chan ->
      let sexps =
        scan (Lexing.from_channel chan) ~fail_on_parse_error:t.fail_on_parse_error
      in
      if t.group then singleton (Sexp_ext.List sexps) else sexps)
  in
  let iter_source label sexps =
    let process_sexp = Transform.initialize_source transform label in
    Lazy_list.iter sexps ~f:(fun sexp -> t.perform_query sexp ~on_result:process_sexp);
    Transform.finalize_source transform label
  in
  Located.iter input ~f:iter_source;
  Transform.finalize_all transform;
  exit (if Transform.any_output transform then 0 else 1)
;;

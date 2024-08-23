open! Core

module Query_args = struct
  type t =
    { output_mode : Query.output_mode
    ; allow_empty_output : bool
    ; labeled : bool option
    }
end

let query_args =
  let%map_open.Command output_mode =
    let%map_open.Command quiet =
      flag
        "quiet"
        no_arg
        ~doc:" Produce no output (useful when running for exit status alone)"
    and count = flag "count" no_arg ~doc:" Produce only a count of returned sexps" in
    match quiet, count with
    | true, false -> Query.Silent
    | false, true -> Query.Count
    | false, false -> Query.Sexp
    | true, true -> failwith "can't pass both -quiet and -count"
  and allow_empty_output =
    flag "allow-empty-output" no_arg ~doc:" Do not fail even if no match is found"
  and labeled =
    let%map_open.Command label =
      flag "label" no_arg ~doc:" pair with filenames (override default behavior)"
    and no_label =
      flag
        "no-label"
        no_arg
        ~doc:" do not pair with filenames (override default behavior)"
    in
    match label, no_label with
    | true, false -> Some true
    | false, true -> Some false
    | false, false -> None
    | true, true -> failwith "can't pass both -label and -no-label flags"
  in
  { Query_args.output_mode; allow_empty_output; labeled }
;;

module Machine_and_fail_on_parse_error = struct
  type t =
    { machine : bool
    ; fail_on_parse_error : bool
    }
end

let machine =
  let open Command.Param in
  flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
;;

let machine_and_fail_on_parse_error =
  let%map_open.Command machine
  and fail_on_parse_error =
    flag
      "fail-on-parse-error"
      no_arg
      ~doc:" raise exception on bad input (override default behavior)"
  in
  { Machine_and_fail_on_parse_error.machine; fail_on_parse_error }
;;

let channel_stdin_or_anon_file =
  let open Command.Param in
  match%map.Command anon (maybe ("FILE" %: Filename_unix.arg_type)) with
  | None -> Stdlib.stdin
  | Some file -> In_channel.create file
;;

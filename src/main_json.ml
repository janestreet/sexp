open Core
open Sexplib
open Composition_infix

let to_json ~in_channel () =
  match
    let%map.List sexp = Sexp.input_sexps in_channel in
    let json = Sexp_app.To_json.json_of_sexp sexp in
    print_endline (Jsonaf.to_string_hum json)
  with
  | (_ : unit list) -> ()
  | exception End_of_file -> ()
;;

let of_json ~in_channel ~machine =
  let module Conv = Sexplib.Conv in
  let rec convert : Jsonaf.t -> Sexp.t = function
    | `Null -> Conv.sexp_of_unit ()
    | `True -> Conv.sexp_of_bool true
    | `False -> Conv.sexp_of_bool false
    | `Number s -> Conv.sexp_of_string s
    | `String s -> Conv.sexp_of_string s
    | `Array json_list -> Conv.sexp_of_list convert json_list
    | `Object alist ->
      let sexp_of_item = Conv.sexp_of_pair Conv.sexp_of_string convert in
      Conv.sexp_of_list sexp_of_item alist
  in
  let parser_state =
    (* Print as we go so the incremental work can be gc'd *)
    Angstrom.lift (convert >> print_s ?mach:(Option.some_if machine ())) Jsonaf.Parser.t
    |> Angstrom.many
    |> Angstrom.map ~f:(fun (_ : unit list) -> ())
    |> Angstrom.Buffered.parse
  in
  let parser_state =
    In_channel.fold_lines ~init:parser_state in_channel ~f:(fun parser_state line ->
      Angstrom.Buffered.feed parser_state (`String line))
  in
  let parser_state = Angstrom.Buffered.feed parser_state `Eof in
  match Angstrom.Buffered.state_to_result parser_state with
  | Error s -> eprint_s [%message "Got an error parsing the json" s]
  | Ok () -> ()
;;

let to_json_command =
  Command.basic
    ~summary:"Convert sexps on stdin or in a file to json."
    (let%map_open.Command in_channel = Shared_params.channel_stdin_or_anon_file in
     fun () -> to_json ~in_channel ())
;;

let of_json_command =
  Command.basic
    ~summary:"Convert json on stdin or in a file to sexps."
    (let%map_open.Command machine = Shared_params.machine
     and in_channel = Shared_params.channel_stdin_or_anon_file in
     fun () -> of_json ~in_channel ~machine)
;;

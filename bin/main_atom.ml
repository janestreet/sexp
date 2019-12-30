open Core
open Async

module Escape = struct
  module Kind = struct
    type t =
      | Exactly_stdin
      | Strip_trailing_newlines
      | Lines
  end

  let command =
    Command.async
      ~summary:"Convert stdin into a sexp atom"
      (let open Command.Let_syntax in
       [%map_open
         let kind =
           choose_one
             ~if_nothing_chosen:(Default_to Kind.Exactly_stdin)
             [ flag
                 "-strip-trailing-newlines"
                 (no_arg_some Kind.Strip_trailing_newlines)
                 ~doc:"ignore any trailing newlines."
             ; flag
                 "-lines"
                 (no_arg_some Kind.Lines)
                 ~doc:
                   "generate and output one atom per line of input. This ignores a \
                    single trailing newline."
             ]
         in
         fun () ->
           let open Deferred.Let_syntax in
           let stdin = Lazy.force Reader.stdin in
           let singleton = Deferred.map ~f:Pipe.singleton in
           let%bind strings =
             match kind with
             | Exactly_stdin -> Reader.contents stdin |> singleton
             | Strip_trailing_newlines ->
               Reader.contents stdin
               |> Deferred.map ~f:(String.rstrip ~drop:(Char.( = ) '\n'))
               |> singleton
             | Lines -> Reader.lines stdin |> return
           in
           Pipe.iter_without_pushback strings ~f:(fun str ->
             str |> Sexp.Atom |> Sexp.to_string |> print_endline)])
  ;;
end

module Unescape = struct
  let command =
    Command.async
      ~summary:"Print unescaped atoms from stdin"
      (let open Command.Let_syntax in
       [%map_open
         let () = return () in
         fun () ->
           Lazy.force Reader.stdin
           |> Reader.read_sexps
           |> Pipe.iter_without_pushback ~f:(function
             | Atom atom -> print_endline atom
             | List _ as input ->
               Core.eprint_s [%message "Non-atom input" (input : Sexp.t)])])
  ;;
end

let command =
  Command.group
    ~summary:"Escape/unscape sexp atoms"
    [ "escape", Escape.command; "unescape", Unescape.command ]
;;

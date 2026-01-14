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
      [%map_open.Command
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
                  "generate and output one atom per line of input. This ignores a single \
                   trailing newline."
            ]
        in
        fun () ->
          let stdin = Lazy.force Reader.stdin in
          let singleton = Deferred.map ~f:Pipe.singleton in
          let%bind strings =
            match kind with
            | Exactly_stdin -> Reader.contents stdin |> singleton
            | Strip_trailing_newlines ->
              Reader.contents stdin
              |> Deferred.map ~f:(fun s -> String.rstrip s ~drop:(Char.( = ) '\n'))
              |> singleton
            | Lines -> Reader.lines stdin |> return
          in
          Pipe.iter_without_pushback strings ~f:(fun str ->
            str |> Sexp.Atom |> Sexp.to_string |> print_endline)]
      ~behave_nicely_in_pipeline:false
  ;;
end

module Escape_command = struct
  let command =
    Command.async
      ~summary:"convert an argument list to a sequence of sexp atoms"
      ~readme:(fun () ->
        "This command is useful when you need to call an OCaml program from bash,\n\
         where OCaml needs a list of strings as a sexp, while the bash program has it\n\
         available in an array.\n\n\
         For example, you can write the argument list to a sexp config file like this:\n\n\
        \  run () {\n\
        \    extra_args=$(sexp atom escape-command -- \"$@\")\n\
        \    echo \"((some_arg 1)(extra_args ($extra_args)))\" >config.sexp\n\
        \    run.exe -config config.sexp\n\
        \  }\n")
      [%map_open.Command
        let args =
          flag "--" ~doc:"args the command line to convert to a sexp list" escape
        in
        fun () ->
          (match args with
           | None -> failwith "no args given to sexp unescape-command"
           | Some args ->
             List.iter args ~f:(fun str ->
               str |> Sexp.Atom |> Sexp.to_string |> print_endline));
          return ()]
      ~behave_nicely_in_pipeline:false
  ;;
end

module Unescape = struct
  let command =
    let parse s = Scanf.unescaped s in
    let unparse c = String.escaped (String.of_char c) in
    Command.async
      ~summary:"Print unescaped atoms from stdin"
      [%map_open.Command
        let () = return ()
        and sep =
          flag_optional_with_default_doc_sexp
            "-sep"
            string
            [%sexp_of: string]
            ~default:"\n"
            ~doc:
              [%string
                "STR separator/terminator to use instead of line breaks. The syntax is \
                 the same as for string literals in OCaml. (e.g. \"%{unparse '\n\
                 '}\", \"%{unparse '\000'}\")"]
        in
        fun () ->
          let sep = parse sep in
          Lazy.force Reader.stdin
          |> Reader.read_sexps
          |> Pipe.iter_without_pushback ~f:(function
            | Atom atom -> printf "%s%s" atom sep
            | List _ as input ->
              Core.eprint_s [%message "Non-atom input" (input : Sexp.t)])]
      ~behave_nicely_in_pipeline:false
  ;;
end

module Unescape_command = struct
  module Input = struct
    type t =
      | Sexp_lists
      | Atoms

    let param =
      let open Command.Param in
      choose_one
        ~if_nothing_chosen:(Default_to Atoms)
        [ flag
            "-sexp-lists"
            (no_arg_some Sexp_lists)
            ~doc:
              " expect the input to be a sequence of sexp lists, one per command line. \
               The command lines will be printed newline-separated."
        ; flag
            "-atoms"
            (no_arg_some Atoms)
            ~doc:
              " expect the input to be a sequence of atoms that comprise a single \
               command line. This is the default."
        ]
    ;;
  end

  let command =
    Command.async
      ~summary:"convert command line(s) specified as sexps to shell syntax"
      ~readme:(fun () ->
        "This command is useful when you have an argument list written in sexp\n\
         format and you want to use it as a part of a command line in bash.\n\
         This avoids the quoting issues that can otherwise be tricky to deal with.\n\
         For example:\n\n\
        \  args='-arg1\"foo\"-arg2\"bar\"'\n\
        \  cmd=\"command.exe $(printf \"%s\" \"$args\" | sexp atom unescape-command)\"\n\
        \  bash -c \"$cmd\"")
      [%map_open.Command
        let input = Input.param in
        fun () ->
          let input_pipe = Lazy.force Reader.stdin |> Reader.read_sexps in
          match input with
          | Sexp_lists ->
            Pipe.iter_without_pushback input_pipe ~f:(function
              | Sexp.Atom _ ->
                raise_s
                  [%sexp
                    "Atom encountered where a command line encoded as a sexp list was \
                     expected."]
              | List l ->
                let atoms = List.map l ~f:[%of_sexp: string] in
                print_endline (String.concat ~sep:" " (List.map ~f:Sys.quote atoms)))
          | Atoms ->
            let first = ref true in
            let%map () =
              Pipe.iter_without_pushback input_pipe ~f:(fun atom ->
                if not !first then print_string " ";
                first := false;
                print_string (Sys.quote ([%of_sexp: string] atom)))
            in
            print_endline ""]
      ~behave_nicely_in_pipeline:false
  ;;
end

let command =
  Command.group
    ~summary:"Escape/unscape sexp atoms"
    [ "escape", Escape.command
    ; "unescape", Unescape.command
    ; "escape-command", Escape_command.command
    ; "unescape-command", Unescape_command.command
    ]
;;

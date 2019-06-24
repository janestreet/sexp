open! Core

(* Options are provided for parameters which are likely to change a lot and are likely to
   vary depending on the file that is being processed.
*)
let command =
  Command.basic
    ~summary:"Pretty print S expressions in a human-friendly way."
    ~readme:(fun () ->
      "Use pre-defined styles or load a custom style from a file."
      ^ "\nYou can use -p to print out one of the predefined styles and customize it.")
    (let open Command.Let_syntax in
     let%map_open config_file =
       flag "-c" (optional Filename.arg_type) ~doc:"file use custom configuration file"
     and color =
       flag
         "-color"
         no_arg
         ~doc:
           (" enable colors. By default, colors are disabled "
            ^ "even if they are set in the configuration file")
     and interpret_atom_as_sexp = flag "-i" no_arg ~doc:" try to interpret atoms as sexps"
     and drop_comments = flag "-drop-comments" no_arg ~doc:" drop comments"
     and new_line_separator =
       flag "-s" (optional bool) ~doc:"bool separate sexps with an empty line"
     and print_settings =
       flag "-p" no_arg ~doc:" print the settings in colorless format"
     in
     fun () ->
       let config =
         match config_file with
         | Some path -> Sexp.load_sexp_conv_exn path Sexp_pretty.Config.t_of_sexp
         | None -> Sexp_pretty.Config.default
       in
       let config =
         let color = color || print_settings in
         Sexp_pretty.Config.update
           config
           ~interpret_atom_as_sexp
           ~drop_comments
           ~color
           ?new_line_separator
       in
       if print_settings
       then (
         let config_for_output =
           { config with
             atom_coloring = Color_none
           ; paren_coloring = false
           ; atom_printing = Escaped
           }
         in
         let fmt = Format.formatter_of_out_channel Caml.stdout in
         let sexp =
           Sexp_pretty.sexp_to_sexp_or_comment (Sexp_pretty.Config.sexp_of_t config)
         in
         Sexp_pretty.Sexp_with_layout.pp_formatter config_for_output fmt sexp)
       else (
         let sparser = Sexp.With_layout.Parser.sexp Sexp.With_layout.Lexer.main in
         let lexbuf = Lexing.from_channel Caml.stdin in
         let fmt = Format.formatter_of_out_channel stdout in
         let next () =
           try Some (sparser lexbuf) with
           | _ -> None
         in
         Sexp_pretty.Sexp_with_layout.pp_formatter' ~next config fmt))
;;

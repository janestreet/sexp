open Core

type source =
  | Anon of string
  | File of string

let load_pattern source =
  match source with
  | Anon prgm -> prgm
  | File file -> In_channel.read_all file
;;

let wrap_mode_param =
  let%map_open.Command wrap_singletons =
    flag
      "wrap-singletons"
      no_arg
      ~doc:"when capturing lists, also wrap single sexps as alist"
  and unwrap_sequence_captures =
    flag
      "unwrap-sequence-captures"
      no_arg
      ~doc:"when capturing a sequence, unwrap into multiple distinct sexps"
  in
  match wrap_singletons, unwrap_sequence_captures with
  | false, false -> Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_non_singletons
  | true, false -> Sexp_app_pattern.Output_method.Wrap_mode.T Wrap_always
  | false, true -> Sexp_app_pattern.Output_method.Wrap_mode.T Unwrap_always
  | true, true ->
    failwith "Cannot specify both -wrap-singletons and -unwrap-sequence-captures"
;;

let pat_query_command =
  Command.basic
    ~summary:"query for parts of an s-expression via regex-like pattern"
    ~readme:(fun () -> Sexp_app_pattern.Help.pat_query_readme)
    (let%map_open.Command () = return ()
     and () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            Core.print_endline Sexp_app_pattern.Help.pat_query_examples;
            Core.exit 0))
         ~doc:" Print a longer explanation with examples"
     and source, inputs, labeled_default =
       let%map_open.Command file =
         flag
           "pattern-file"
           (optional Filename_unix.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       and pattern_and_files =
         anon
           (maybe
              (t2 ("PATTERN" %: string) (sequence ("FILE" %: Filename_unix.arg_type))))
       and stdin_label =
         flag
           "stdin-label"
           (optional string)
           ~doc:"LABEL override default label for stdin"
       in
       let pattern, files =
         match pattern_and_files with
         | None -> None, []
         | Some (pattern, files) -> Some pattern, files
       in
       let inputs, labeled_default =
         match files with
         | [] -> Located.stdin stdin_label, Option.is_some stdin_label
         | [ file ] -> Located.files [ file ], false
         | files -> Located.files files, true
       in
       let source =
         match file, pattern with
         | Some x, None -> File x
         | None, Some x -> Anon x
         | _ -> failwith "must pass exactly one of PATTERN or -pattern-file"
       in
       source, inputs, labeled_default
     and wrap_mode = wrap_mode_param
     and { output_mode; allow_empty_output; labeled } = Shared_params.query_args
     and { machine; fail_on_parse_error } = Shared_params.machine_and_fail_on_parse_error
     and format =
       flag
         "format"
         (optional sexp)
         ~doc:"SEXPLIKE explicitly say how to format the captures of a pattern"
     in
     fun () ->
       let pattern = load_pattern source in
       let query = Sexp_app_pattern.Parser.parse_exn pattern in
       let output_method =
         let (Sexp_app_pattern.Output_method.Wrap_mode.T wrap_mode) = wrap_mode in
         match format with
         | None -> Sexp_app_pattern.Output_method.default_method query ~wrap_mode
         | Some format ->
           Sexp_app_pattern.Output_method.T
             (Sexp_app_pattern.Output_method.Formats
                (wrap_mode, [ Sexp_app_pattern.Output_method.Format.t_of_sexp format ]))
       in
       let perform_query_returning_sexps
         ~(output_method : Sexp.t Sexp_app_pattern.Output_method.t)
         sexp_ext
         ~(on_result : Sexp.t -> unit)
         =
         let sexp = Sexp_app.Sexp_ext.sexp_of_t sexp_ext in
         Sexp_app_pattern.Engine.iter_matches ~query ~output_method sexp ~f:(fun output ->
           on_result output)
       in
       let perform_query_returning_sexp_lists
         ~(output_method : Sexp.t list Sexp_app_pattern.Output_method.t)
         sexp_ext
         ~(on_result : Sexp.t -> unit)
         =
         let sexp = Sexp_app.Sexp_ext.sexp_of_t sexp_ext in
         Sexp_app_pattern.Engine.iter_matches ~query ~output_method sexp ~f:(fun output ->
           List.iter ~f:on_result output)
       in
       let perform_query =
         let (Sexp_app_pattern.Output_method.T output_method) = output_method in
         match output_method with
         | Formats _ as output_method -> perform_query_returning_sexp_lists ~output_method
         | List _ as output_method -> perform_query_returning_sexps ~output_method
         | Record _ as output_method -> perform_query_returning_sexps ~output_method
         | Single_capture Unwrap_always as output_method ->
           perform_query_returning_sexp_lists ~output_method
         | Single_capture Wrap_non_singletons as output_method ->
           perform_query_returning_sexps ~output_method
         | Single_capture Wrap_always as output_method ->
           perform_query_returning_sexps ~output_method
         | Map -> assert false
       in
       Query.execute
         { inputs
         ; output_mode
         ; allow_empty_output
         ; group = false
         ; machine
         ; labeled = Option.value labeled ~default:labeled_default
         ; fail_on_parse_error
         ; perform_query
         })
;;

let pat_change_readme () =
  {|
Replace parts of sexps via regex-like patterns. Basic usage:

./sexp pat-change PATTERN -replace %CAPTURE -with REPLACEMENT

For each sexp and each possible way that PATTERN can match that sexp, replace the subsexp
that %CAPTURE corresponded to in that match with REPLACEMENT, where REPLACEMENT can be
in terms of captured values.

%CAPTURE should be the named capture that exists within PATTERN to be replaced, and
pat-change will fail if PATTERN has no such named capture.

See -help or -examples for the 'pat-query' command for a general explanation of how
patterns and capturing works. Other than that, if you understand pat-query, then some
examples should hopefully illustrate how pat-change works. So see -examples for a few
examples.

And for more precise specification of the semantics of query patterns and the underlying
type that corresponds to the grammar, see:
https://ocaml.janestreet.com/ocaml-core/latest/doc/sexp_app_pattern/Sexp_app_pattern/Query/index.html

|}
;;

let pat_change_examples =
  {|
A few examples of sexp pat-change!

----------------------------------------------
Remove a record field foo

sexp pat-change ".. (.. %field=(foo .))" \
-replace %field \
-with-nothing

Effect:
((foo 1)(bar 2)(list (foo 1 2)))
->
((bar 2)(list (foo 1 2)))

----------------------------------------------
Rename a record field foo to baz

sexp pat-change ".. (.. %field=(foo %value))" \
-replace %field \
-with "(baz %value)"

Effect:
((foo 1)(bar 2)(list (foo 1 2)))
->
((baz 1)(bar 2)(list (foo 1 2)))

----------------------------------------------
Add a record field baz with value 0 after record field foo

sexp pat-change ".. (.. %field=(foo %value))" \
-replace %field \
-with "%field (baz 0)"

Effect:
((foo 1)(bar 2)(list (foo 1 2)))
->
((foo 1)(baz 0)(bar 2)(list (foo 1 2)))

----------------------------------------------
Reverse the order of all pairs.

sexp pat-change ".. %0=(%foo %bar)" \
-replace %0 \
-with "(%bar %foo)"

Effect:
((a 1)(b 2)(c 3))
->
((1 a)(2 b)(3 c))

Effect (pat-change does NOT recurse through a subsexp itself already being replaced):
((a 1)(b 2))
->
((b 2)(a 1))

----------------------------------------------
Replace all instances of a record with a particular field "foo" with a tuple that
indicates the former value of that field.

sexp pat-change ".. %record={(foo %x)}" \
-replace %record \
-with "(got %x)"

(foo a)
((bar b)(foo c))
((foo (foo d))(baz e))
((goo (foo f))(baz g))
->
(foo a)          # no replacement
(got c)          # replacement occurred
(got (foo d))    # outermost replacement occurred among two possible replacements
((got f)(baz g)) # inner replacement occurred

---
Perform some more complex processing where the replacement involves a capture
outside of the sexp being replaced.

sexp pat-change "{(label %label) .. %a={ (id %id) (counts %counts) }}" \
-replace "%a" \
-with "(%label %id %counts)"

Effect:
((label B) (
   ((id X) (counts (1 2 3)))
   ((id Y) (counts (4 5 6))) ))
->
((label B) (
   (B X (1 2 3))
   (B Y (4 5 6)) ))
|}
;;

let pat_change_command =
  Command.basic
    ~summary:"change parts of an s-expression via regex-like pattern"
    ~readme:pat_change_readme
    (let%map_open.Command () = return ()
     and () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            Core.print_endline pat_change_examples;
            Core.exit 0))
         ~doc:" Print a longer explanation with examples"
     and { machine; fail_on_parse_error } = Shared_params.machine_and_fail_on_parse_error
     and source, files =
       let%map_open.Command x =
         anon
           (maybe
              (t2 ("PATTERN" %: string) (sequence ("FILE" %: Filename_unix.arg_type))))
       and file =
         flag
           "pattern-file"
           (optional Filename_unix.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       in
       match x, file with
       | None, Some x -> File x, []
       | Some (prgm, files), None -> Anon prgm, files
       | _ -> failwith "must pass exactly one of -pattern-file and PATTERN"
     and stdin_label =
       flag "stdin-label" (optional string) ~doc:"LABEL override default label for stdin"
     and wrap_mode = wrap_mode_param
     and replace =
       flag "replace" (required string) ~doc:"%foo what named capture location to replace"
     and format =
       choose_one
         ~if_nothing_chosen:Raise
         [ flag
             "with"
             (optional string)
             ~doc:"SEXPLIKES what to replace with, in terms of captures"
           |> map ~f:(Option.map ~f:(fun format -> `Format format))
         ; flag
             "with-nothing"
             ~full_flag_required:()
             (no_arg_some `Delete)
             ~doc:"Delete the capture; equivalent to -with ''"
         ]
     in
     fun () ->
       let inputs, labeled =
         match files with
         | [] -> Located.stdin stdin_label, Option.is_some stdin_label
         | [ file ] -> Located.files [ file ], false
         | files -> Located.files files, true
       in
       let pattern = load_pattern source in
       let query = Sexp_app_pattern.Parser.parse_exn pattern in
       let formats =
         match format with
         | `Format format -> Sexp_app_pattern.Output_method.Format.ts_of_string format
         | `Delete -> []
       in
       let perform_query sexp_ext ~on_result =
         let (Sexp_app_pattern.Output_method.Wrap_mode.T wrap_mode) = wrap_mode in
         let sexp = Sexp_app.Sexp_ext.sexp_of_t sexp_ext in
         let sexps =
           Sexp_app_pattern.Engine.replace ~query ~replace ~with_:formats sexp ~wrap_mode
         in
         List.iter ~f:on_result sexps
       in
       Query.execute
         { inputs
         ; output_mode = Sexp
         ; allow_empty_output = false
         ; group = false
         ; machine
         ; labeled
         ; fail_on_parse_error
         ; perform_query
         })
;;

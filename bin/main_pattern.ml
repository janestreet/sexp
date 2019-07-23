open Core

let pat_query_readme =
  {|
=== Pattern syntax summary ===
For a longer explanation with examples, run with -examples.

.             : Matches any sexp
abc           : Matches atom 'abc'
/regex/       : Matches atom via regular expression
foo*          : Matches foo zero or more times
foo+          : Matches foo one or more times
foo?          : Matches foo zero or one times
( )           : Matches a sexp list as a list, enforcing order and exact length
{ }           : Matches a sexp list as a set, not enforcing order AND allowing extra elements.
[ ]           : Limit the scope of things (see examples below)

.. foo        : Searches for foo anywhere within any subexpression of the sexp.
              : This is a lot like sexp query's "smash".

%.            : Simple capture
%(a %. c)     : Simple captures can also be put on other subpatterns, or nested.
%0 %1         : Numbered captures
%abc %def     : Named captures
%abc=(a %. c) : Named or numbered captures can also be used on subpatterns or nested.

a bc | de f   : Matches 'a bc' OR 'de f'. Shortcircuiting.
a [bc | de] f : Matches 'a', then 'bc' OR 'de', then 'f'.
a b & c d     : Matches any sexp that matches both 'a b' AND 'c d'.

[.. a & .. b] : Matches any sexp that has 'a' anywhere within, AND has 'b' anywhere within.

!foo          : Stop matching after the first match that foo finds.
|}
;;

let pat_query_examples =
  {|
HOW TO USE:

Write a sexp-like expression for the pattern you're trying to match and use "%." to
capture a value you want.

Example pattern: (a b %.)
Effect:
(a b c)      -> c
(a b ())     -> ()
(a b (c d))  -> (c d)
(a b c d)    ->  <<no match>>
(a b)        ->  <<no match>>

To capture multiple values into a list, you can use numbered captures (zero-indexed).
Example pattern: (%0 b %1)
Effect:
(a b c) -> (a c)

Or you can capture them into records by using named captures.
Example pattern: (%foo b %bar)
Effect:
(a b c) -> ((foo a) (bar c))

You can also use -format to specify an explicit output format, like:
Example pattern: (%foo b %bar), with -format (%foo (abc %bar))
Effect:
(a b c) -> (a (abc c))

Regular parens require an exact match in order and length, but you can use curly brackets
if you need to be robust to the order that things appear in something like a record, and
you don't care if there are other fields:
Example pattern: { (name %0) (time %1) }
Effect:
((name Alice) (time 9:00) (qty 3)) -> (Alice 9:00)
((time 10:00) (name Bob))          -> (Bob 10:00)

Most of the time, if you have a big sexp and you only want some deep piece of it,
you can use '..', which will descend deep into any subsexp and return all matches.
Example pattern:  .. (sym %.)
Effect:
(my giant sexp ... (( more stuff ... (sym foo)...)) ... (sym bar) ... )
->
foo            <---- Two separate matches
bar            <---/

'..' also works within subexpressions. For example, the following would search for a
subrecord with a field "id", and anywhere deeper in it for a field "routes", and capture
all the routes by id:

Example pattern: .. { (id %0) .. (routes { %1 }) }
Effect:
(some giant sexp... ( ((id FOO) ((... (routes (A B)))))
                      ((id BAR) ((... (routes (A E))))) ))
->
(FOO A)
(FOO B)
(BAR A)
(BAR E)

You can use '.' to match any value, and * to match something zero or more times.
For example, the following would grab the second element out of any subsexp:
Example pattern: .. (. %. .*)
Effect:
(a (b c) (d e f))
->
(b c)
c
e

You can use '?' to express that a value might or might not be there. For example,
if you have two fields that are both optional:
Example pattern: { (start (%0?)) (stop (%1?)) }
Effect:
((start (3)) (stop (4))) -> (3 4)
((start ()) (stop (4)))  -> (() 4)
((start (3)) (stop ()))  -> (3 ())
((start ()) (stop ()))   -> (() ())

You can use '&' for AND and '|' for OR.
Use square brackets to delimit the scope of things where needed.
Examples:

Search anywhere for a triple of a, then b OR c, then d, and capture the b OR c:
.. (a %[b | c] d)

Search separately anywhere for a field "foo" and anywhere for a field "bar", and return
the cross product of all the things they match:
.. (foo %foo) & .. (bar %bar)

You can use % on whole sexps as well. For example, to capture all records that have
the field "count" anywhere ("." matches any sexp):

.. %{(count .)}

Or to capture it in a named or numbered fashion:
.. %foo={(count .)}

That's most of the basic things you can do!

|}
;;

type source =
  | Anon of string
  | File of string

let load_pattern source =
  match source with
  | Anon prgm -> prgm
  | File file -> In_channel.read_all file
;;

let pat_query_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"query for parts of an s-expression via regex-like pattern"
    ~readme:(fun () -> pat_query_readme)
    (let%map_open () = return ()
     and () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            Core.print_endline pat_query_examples;
            Core.exit 0))
         ~doc:" Print a longer explanation with examples"
     and source, inputs, labeled_default =
       let%map_open file =
         flag
           "pattern-file"
           (optional Filename.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       and pattern_and_files =
         anon (maybe (t2 ("PATTERN" %: string) (sequence ("FILE" %: Filename.arg_type))))
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
     and wrap_singletons =
       flag
         "wrap-singletons"
         no_arg
         ~doc:"when capturing lists, also wrap single sexps as a list"
     and output_mode =
       let%map_open quiet =
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
       let%map_open label =
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
     and machine =
       flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
     and fail_on_parse_error =
       flag
         "fail-on-parse-error"
         no_arg
         ~doc:" raise exception on bad input (override default behavior)"
     and format =
       flag
         "format"
         (optional sexp)
         ~doc:"SEXPLIKE explicitly say how to format the captures of a pattern"
     in
     fun () ->
       let pattern = load_pattern source in
       let query = Sexp_app_pattern.Parser.parse_exn pattern in
       let perform_query'
             output_method
             process_output
             sexp_ext
             ~(on_result : Sexp.t -> unit)
         =
         let sexp = Sexp_app.Sexp_ext.sexp_of_t sexp_ext in
         Sexp_app_pattern.Engine.iter_matches
           ~query
           ~output_method
           sexp
           ~wrap_singletons
           ~f:(fun output -> process_output on_result output)
       in
       let perform_query =
         match format with
         | None -> perform_query' Default (fun on_result sexp -> on_result sexp)
         | Some format ->
           perform_query'
             ([ Sexp_app_pattern.Output_method.Format.t_of_sexp format ] |> Formats)
             (fun on_result sexps -> List.iter ~f:on_result sexps)
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

|}
;;

let pat_change_examples =
  {|
A few examples of sexp pat-change!

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
  let open Command.Let_syntax in
  Command.basic
    ~summary:"change parts of an s-expression via regex-like pattern"
    ~readme:pat_change_readme
    (let%map_open () = return ()
     and () =
       flag
         "examples"
         (no_arg_abort ~exit:(fun () ->
            Core.print_endline pat_change_examples;
            Core.exit 0))
         ~doc:" Print a longer explanation with examples"
     and machine =
       flag "machine" no_arg ~doc:" Use machine style for output (one sexp per line)"
     and fail_on_parse_error =
       flag
         "fail-on-parse-error"
         no_arg
         ~doc:" raise exception on bad input (override default behavior)"
     and source, files =
       let%map_open x =
         anon (maybe (t2 ("PATTERN" %: string) (sequence ("FILE" %: Filename.arg_type))))
       and file =
         flag
           "pattern-file"
           (optional Filename.arg_type)
           ~doc:"FILE Read program from file instead of command line"
       in
       match x, file with
       | None, Some x -> File x, []
       | Some (prgm, files), None -> Anon prgm, files
       | _ -> failwith "must pass exactly one of -pattern-file and PATTERN"
     and stdin_label =
       flag "stdin-label" (optional string) ~doc:"LABEL override default label for stdin"
     and wrap_singletons =
       flag
         "wrap-singletons"
         no_arg
         ~doc:"when capturing lists, also wrap single sexps as alist"
     and replace =
       flag
         "replace"
         (required string)
         ~doc:"%foo what named capture location to replace"
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
         let sexp = Sexp_app.Sexp_ext.sexp_of_t sexp_ext in
         let sexps =
           Sexp_app_pattern.Engine.replace
             ~query
             ~replace
             ~with_:formats
             sexp
             ~wrap_singletons
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

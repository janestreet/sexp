open Core

(* Given a function to get the next char of the input, returns a function to get the next
   block of transformed input. Strings returned via `Ok always have nonzero length. *)
let read_of_next_char
  : next_char:(unit -> char option) -> (unit -> [ `Ok of string | `Eof ]) Staged.t
  =
  fun ~next_char ->
  (* These are string that could trigger comment-mode in the sexp lexer OR lead to parse
     errors - we make sure they get quoted so that they get interpreted as atoms instead *)
  let should_be_quoted c =
    match c with
    | ';' | '|' | '#' | ')' -> true
    | _ -> false
  in
  (* The transformation necessary to turn a raw atom that didn't appear with double quotes
     into a string that will parse to the same character sequence once it does get
     double-quoted *)
  let escape = unstage (String.Escaping.escape ~escapeworthy:[ '"' ] ~escape_char:'\\') in
  let maybe_quote_not_inside_string_atom s =
    if String.exists s ~f:should_be_quoted then "\"" ^ escape s ^ "\"" else s
  in
  (* These are characters that signal the end of the current atom when not inside a string *)
  let terminates_atom c ~paren_depth =
    match c with
    | '(' | '"' | ' ' | '\t' | '\012' | '\n' | '\r' -> true
    | ')' when Int.( > ) !paren_depth 0 -> true
    | _ -> false
  in
  (* State variables *)
  let paren_depth = ref 0 in
  let inside_string = ref false in
  let follows_escape_in_string = ref false in
  let atom_so_far = Buffer.create 32 in
  let all_done = ref false in
  (* Read from the in_channel and either return `Eof or return `Ok s where s is a chunk of
     input, possibly zero-length *)
  let read () =
    if !all_done
    then `Eof
    else (
      match next_char () with
      | Some c ->
        if (* Inside string *)
           !inside_string
        then (
          (* If we followed an escape character, we always take the next char verbatim *)
          let followed_escape_in_string = !follows_escape_in_string in
          follows_escape_in_string := false;
          if followed_escape_in_string
          then (
            Buffer.add_char atom_so_far c;
            `Ok "" (* Else... *))
          else (
            match c with
            (* A quote terminates the string and we return it *)
            | '"' ->
              Buffer.add_char atom_so_far c;
              let s = Buffer.contents atom_so_far in
              Buffer.clear atom_so_far;
              inside_string := false;
              `Ok s
            (* Any other character gets added to the string, and if it's an escape
               character, we remember this *)
            | c ->
              if Char.equal c '\\' then follows_escape_in_string := true;
              Buffer.add_char atom_so_far c;
              `Ok "" (* Not inside string *)))
        else if (* Chars that don't terminate the atom just get appended and we continue *)
                not (terminates_atom c ~paren_depth)
        then (
          Buffer.add_char atom_so_far c;
          `Ok "" (* Else... *))
        else (
          (* We have a naked atom that didn't appear as a string in the sexp - quote if it
             needed *)
          let ret = Buffer.contents atom_so_far in
          Buffer.clear atom_so_far;
          let ret = maybe_quote_not_inside_string_atom ret in
          (* Then handle the character that terminated the atom *)
          match c with
          (* Parens change the depth and then get output *)
          | '(' ->
            incr paren_depth;
            `Ok (ret ^ String.of_char c)
          | ')' ->
            decr paren_depth;
            `Ok (ret ^ String.of_char c)
          (* Whitespace simply gets output *)
          | ' ' | '\t' | '\012' | '\n' | '\r' -> `Ok (ret ^ String.of_char c)
          (* Quotes send us into string mode *)
          | '"' ->
            inside_string := true;
            Buffer.add_char atom_so_far c;
            `Ok ret
          | _ -> assert false)
      (* End of in-channel input *)
      | None ->
        let ret =
          (* If inside a string, then to prevent parse errors, finish up the string *)
          if !inside_string
          then (
            (* If there was an escape char without anything after it, complete that too *)
            if !follows_escape_in_string then Buffer.add_char atom_so_far '\\';
            Buffer.add_char atom_so_far '"';
            let ret = Buffer.contents atom_so_far in
            Buffer.clear atom_so_far;
            ret
            (* Else if not inside a string, finish up any naked atom and quote as needed *))
          else (
            let ret = Buffer.contents atom_so_far in
            Buffer.clear atom_so_far;
            maybe_quote_not_inside_string_atom ret)
        in
        (* Then add parens to get our paren depth back to 0 *)
        while !paren_depth > 0 do
          Buffer.add_char atom_so_far ')';
          decr paren_depth
        done;
        (* Yay! *)
        all_done := true;
        `Ok (ret ^ Buffer.contents atom_so_far))
  in
  (* Transform the step function so that it never returns Ok "" *)
  let rec read_until () =
    match read () with
    | `Ok "" -> read_until ()
    | `Ok s -> `Ok s
    | `Eof -> `Eof
  in
  stage read_until
;;

let lexbuf_of_channel chan =
  let next_char () = In_channel.input_char chan in
  let read = unstage (read_of_next_char ~next_char) in
  (* Tuple of string, chars used in string *)
  let leftover = ref ("", 0) in
  (* Read up to n chars into bytes, for lexer *)
  let lex_fun bytes n =
    let result =
      if String.length (fst !leftover) - snd !leftover > 0
      then (
        let s = !leftover in
        leftover := "", 0;
        `Ok s)
      else (
        match read () with
        | `Eof -> `Eof
        | `Ok s -> `Ok (s, 0))
    in
    match result with
    | `Eof -> 0
    | `Ok (s, used) ->
      if String.length s - used > n
      then (
        Bytes.From_string.blit ~src_pos:used ~dst_pos:0 ~src:s ~dst:bytes ~len:n;
        leftover := s, used + n;
        n)
      else (
        Bytes.From_string.blit
          ~src_pos:used
          ~dst_pos:0
          ~src:s
          ~dst:bytes
          ~len:(String.length s - used);
        String.length s - used)
  in
  Lexing.from_function lex_fun
;;

let transform_string s =
  let pos = ref 0 in
  let next_char () =
    if !pos >= String.length s
    then None
    else (
      let c = s.[!pos] in
      incr pos;
      Some c)
  in
  let read = unstage (read_of_next_char ~next_char) in
  let buf = Buffer.create (String.length s) in
  let rec loop () =
    match read () with
    | `Eof -> Buffer.contents buf
    | `Ok s ->
      Buffer.add_string buf s;
      loop ()
  in
  loop ()
;;

open String.Replace_polymorphic_compare

let unchanged s = transform_string s = s
let%test _ = unchanged ""
let%test _ = unchanged "abc"
let%test _ = unchanged "()"
let%test _ = unchanged "bf((a)d((c\"eg\")))"
let%test _ = unchanged " d ( ef) \n (\r\t ) \\ \\m  x \") \b\r (\""
let%test _ = unchanged "%!@&*^:'?/,.~`[}]{-+=_-"
let%test _ = unchanged "\"foo\\\"d\""
let%test "completes unmatched parens" = transform_string "(" = "()"
let%test "completes unmatched parens" = transform_string "(a)(b(()(c" = "(a)(b(()(c)))"
let%test "completes unmatched quotes" = transform_string "\"" = "\"\""
let%test "completes unmatched quotes" = transform_string "\"\\\"" = "\"\\\"\""
let%test "completes unmatched quotes" = transform_string "((\"ab" = "((\"ab\"))"
let%test "completes unmatched escape in string" = transform_string "\"\\" = "\"\\\\\""
let%test "stringifies extra close parens" = transform_string ")" = "\")\""

let%test "stringifies extra close parens" =
  transform_string ")(())))())" = "\")\"(())\"))\"()\")\""
;;

let%test "turns sexp special chars to strings" = transform_string "#" = "\"#\""
let%test "turns sexp special chars to strings" = transform_string ";" = "\";\""
let%test "turns sexp special chars to strings" = transform_string "|" = "\"|\""

let%test "turns sexp special chars to strings" =
  transform_string "## |#| (#a;) ;a\"bc\"|\n;#)|"
  = "\"##\" \"|#|\" (\"#a;\") \";a\"\"bc\"\"|\"\n\";#)|\""
;;

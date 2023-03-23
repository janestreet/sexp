open Core

let main () =
  let buf_size = 1024 in
  let buf = Bytes.create buf_size in
  let idx = ref 0 in
  let n = ref 0 in
  let next_char_no_check () =
    let c = Bytes.get buf !idx in
    incr idx;
    Some c
  in
  let next_char () =
    if !idx >= !n
    then (
      idx := 0;
      n := In_channel.input In_channel.stdin ~buf ~pos:0 ~len:buf_size;
      if !n <= 0 then None else next_char_no_check ())
    else next_char_no_check ()
  in
  let read = unstage (Sexp_app.Parse_everything.read_of_next_char ~next_char) in
  let rec loop () =
    match read () with
    | `Eof -> ()
    | `Ok s ->
      print_string s;
      loop ()
  in
  loop ()
;;

let command =
  let readme () =
    String.strip
      {|
Make a best effort to transform a string into something that will parse as a
sexp, preserving the sexp structure of any parts of the string that already look like a
sexp. Transforms things that would be comments into actual data in the sexp as well.
|}
  in
  Command.basic
    ~summary:"Sexpify an arbitrary string received via stdin."
    ~readme
    (let%map_open.Command () = return () in
     fun () -> main ())
;;

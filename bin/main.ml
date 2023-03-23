open Async

let () =
  Writer.behave_nicely_in_pipeline ();
  Command_unix.run Sexp_cmds.command
;;

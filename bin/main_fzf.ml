open Core
open Async

let main ~filename =
  Sys.command
    ("cat "
     ^ Sys.quote filename
     ^ {| | fzf --multi --no-sort --reverse --exact --ansi --preview-window=down:70% \
        --preview "printf '%s' {} | sexp pp -color" |}
    )
  >>= exit
;;

let command =
  Command.async_or_error
    ~summary:"select one or more inputs sexps with [fzf --multi]"
    [%map_open.Command
      let filename =
        anon (maybe_with_default "-" ("FILENAME" %: Filename_unix.arg_type))
      in
      fun () -> main ~filename]
;;

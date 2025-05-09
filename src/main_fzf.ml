open! Core
open Async

let main ~filename =
  let fzf =
    let fzf_binary = "fzf" in
    let fzf_arguments =
      [ "--multi"
      ; "--track"
      ; "--no-sort"
      ; "--reverse"
      ; "--exact"
      ; "--ansi"
      ; "--preview-window down:70%"
      ; "--preview-label-pos bottom"
      ; "--preview-label 'ctrl+l less | ctrl+alt+c copy | shift+up top | shift+down \
         bottom'"
      ; {| --preview "printf '%s' {} | sexp pp -color -i" |}
      ; {| --bind "ctrl-l:execute(printf '%s' {} | sexp pp -color -i | less -R)" |}
      ; {| --bind "ctrl-alt-c:execute-silent(printf '%s' {} | sexp pp -i | xclip -selection clipboard)" |}
      ; {| --bind "shift-up:first" |}
      ; {| --bind "shift-down:last" |}
      ]
    in
    [%string {| %{fzf_binary} %{String.concat fzf_arguments ~sep:" "} |}]
  in
  Sys.command [%string {| cat %{Sys.quote filename} | %{fzf} |}] >>= exit
;;

let command =
  Command.async_or_error
    ~summary:"select one or more inputs sexps with [fzf --multi]"
    [%map_open.Command
      let filename =
        anon (maybe_with_default "-" ("FILENAME" %: Filename_unix.arg_type))
      in
      fun () -> main ~filename]
    ~behave_nicely_in_pipeline:false
;;

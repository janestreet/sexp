open Core

let summary = "the s-expression toolkit"

let command =
  Command.group
    ~summary
    [ "assemble", Main_parts.assemble_command
    ; "atomize", Main_atomize.command
    ; "change", Main_query.change_command
    ; "diff", Main_diff.diff_command
    ; "flatten", Main_parts.flatten_command
    ; "get", Main_get.command
    ; "multi-select", Main_select.multi_command
    ; "of-xml", Main_of_xml.command
    ; "pat-change", Main_pattern.pat_change_command
    ; "pat-query", Main_pattern.pat_query_command
    ; "patch", Main_diff.patch_command
    ; "pp", Main_pp.command
    ; "print", Main_print.command
    ; "query", Main_query.query_command
    ; "resolve-macros", Main_resolve_macros.command
    ; "restructure", Main_restructure.command
    ; "select", Main_select.command
    ; "sexpify", Main_sexpify.command
    ; "to-csv", Main_to_csv.command
    ; "validate", Main_validate.command
    ]
;;

let () = Command.run command

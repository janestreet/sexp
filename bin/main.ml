open Core

let summary = "the s-expression toolkit"

let command =
  Command.group
    ~summary
    [ "get", Main_get.command
    ; "query", Main_query.command
    ; "pp", Main_pp.command
    ; "print", Main_print.command
    ; "validate", Main_validate.command
    ; "sexpify", Main_sexpify.command
    ; "change", Main_change.command
    ; "to-csv", Main_to_csv.command
    ; "of-xml", Main_of_xml.command
    ; "flatten", Main_parts.flatten_command
    ; "assemble", Main_parts.assemble_command
    ; "select", Main_select.command
    ; "multi-select", Main_select.multi_command
    ; "restructure", Main_restructure.command
    ; "resolve-macros", Main_resolve_macros.command
    ]
;;

let () = Command.run command

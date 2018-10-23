open Core

let summary = "the s-expression toolkit"

let command =
  Command.group
    ~summary
    [ "assemble", Main_parts.assemble_command
    ; "change", Main_change.command
    ; "flatten", Main_parts.flatten_command
    ; "get", Main_get.command
    ; "multi-select", Main_select.multi_command
    ; "of-xml", Main_of_xml.command
    ; "pp", Main_pp.command
    ; "print", Main_print.command
    ; "query", Main_query.command
    ; "resolve-macros", Main_resolve_macros.command
    ; "restructure", Main_restructure.command
    ; "select", Main_select.command
    ; "sexpify", Main_sexpify.command
    ; "to-csv", Main_to_csv.command
    ; "validate", Main_validate.command
    ]
;;

let () = Command.run command

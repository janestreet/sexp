open Core
module Main_group = Main_group

let summary = "the s-expression toolkit"

let command =
  Command.group
    ~summary
    ([ "assemble", Main_parts.assemble_command
     ; "atom", Main_atom.command
     ; "change", Main_query.change_command
     ; "rewrite", Main_query.rewrite_command
     ; "diff", Main_diff.diff_command
     ; "flatten", Main_parts.flatten_command
     ; "fzf", Main_fzf.command
     ; "get", Main_get.command
     ; "multi-select", Main_select.multi_command
     ; "of-json", Main_json.of_json_command
     ; "of-xml", Main_of_xml.command
     ; "pat-change", Main_pattern.pat_change_command
     ; "pat-query", Main_pattern.pat_query_command
     ; "patch", Main_diff.patch_command
     ; "pp", Main_pp.command ~alias_for:"[sexp pretty]" ()
     ; "pretty", Main_pp.command ()
     ; "print", Main_print.command
     ; "query", Main_query.query_command
     ; "resolve-macros", Main_resolve_macros.command
     ; "restructure", Main_restructure.command
     ; "select", Main_select.command
     ; "sexpify", Main_sexpify.command
     ; "sort", Main_sort.command
     ; "to-csv", Main_to_csv.command
     ; "to-json", Main_json.to_json_command
     ; "validate", Main_validate.command
     ]
     @ []
     @ if am_running_test then [ "group", Main_group.command ] else [])
;;

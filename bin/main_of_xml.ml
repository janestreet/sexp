open Core

module Main = struct
  open Xml_light

  type attributes = (string * string) list [@@deriving sexp]

  type xml = Xml.xml =
    | Element of (string * attributes * xml list)
    | PCData of string

  type sexp = Sexp.t =
    | Atom of string
    | List of sexp list

  module type S = sig
    val sexp_of_xml : xml -> sexp
  end

  (* direct style *)
  module _ : S = struct
    let rec sexp_of_xml = function
      | PCData x -> Atom x
      | Element (tag, attrs, children) ->
        let children = List.map ~f:sexp_of_xml children in
        List [ Atom tag; sexp_of_attributes attrs; List children ]
    ;;
  end

  (* continuation-passing style *)
  module _ : S = struct
    let rec sexp_of_xml xml k =
      match xml with
      | PCData x -> k (Atom x)
      | Element (tag, attrs, children) ->
        sexps_of_xmls children (fun children ->
          k (List [ Atom tag; sexp_of_attributes attrs; List children ]))

    and sexps_of_xmls xmls k =
      match xmls with
      | [] -> k []
      | xml :: xmls ->
        sexp_of_xml xml (fun sexp -> sexps_of_xmls xmls (fun sexps -> k (sexp :: sexps)))
    ;;

    (** shadows auxilliary *)
    let sexp_of_xml xml = sexp_of_xml xml Fn.id
  end

  (* defunctionalized continuation-passing style *)
  module V3 : S = struct
    type cont =
      | Top
      | Foo of cont2 * xml list

    and cont2 =
      | Bar of cont2 * sexp
      | Quz of cont * string * sexp

    let rec sexp_of_xml xml k =
      match xml with
      | PCData x -> apply k (Atom x)
      | Element (tag, attrs, children) ->
        sexps_of_xmls children (Quz (k, tag, sexp_of_attributes attrs))

    and sexps_of_xmls xmls k =
      match xmls with
      | [] -> apply2 k []
      | xml :: xmls -> sexp_of_xml xml (Foo (k, xmls))

    and apply k sexp =
      match k with
      | Top -> sexp
      | Foo (k, xmls) -> sexps_of_xmls xmls (Bar (k, sexp))

    and apply2 k sexps =
      match k with
      | Bar (k, sexp) -> apply2 k (sexp :: sexps)
      | Quz (k, tag, attrs) -> apply k (List [ Atom tag; attrs; List sexps ])
    ;;

    let sexp_of_xml xml = sexp_of_xml xml Top

    (* shadows auxilliary *)
  end

  let string_of_sexp ~machine =
    if machine then Sexp.to_string else fun x -> Sexp.to_string_hum x
  ;;

  let main ~machine =
    try
      print_endline
        (string_of_sexp ~machine (V3.sexp_of_xml (Xml.parse_in In_channel.stdin)))
    with
    | Xml_light.Xml.Error xml_err ->
      eprintf "ERROR: %s\n" (Xml_light.Xml.error xml_err);
      exit 1
    | e -> raise e
  ;;
end

let command =
  Command.basic
    ~summary:"convert XML from stdin into an s-expression"
    (let%map_open.Command machine = Shared_params.machine in
     fun () -> Main.main ~machine)
;;

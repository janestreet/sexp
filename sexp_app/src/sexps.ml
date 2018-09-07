open Core

include Hash_set.Make (struct
    type t = Sexp.t =
      | Atom of string
      | List of t list
    [@@deriving compare]

    let sexp_of_t = Fn.id
    let t_of_sexp = Fn.id

    let rec hash = function
      | Atom s -> String.hash s
      | List ts -> List.fold ts ~init:(List.length ts) ~f:(fun n t -> n lxor hash t)
    ;;
  end)

(** hide optional args *)
let create () = create ()

let of_list sexps = of_list sexps

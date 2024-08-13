open! Core
open! Import

let forall_sexps here ~f =
  require_does_not_raise ~cr:CR_soon ~here (fun () ->
    Quickcheck.test
      ~sexp_of:[%sexp_of: Sexp.t]
      ~shrinker:[%quickcheck.shrinker: Sexp.t]
      [%quickcheck.generator: Sexp.t]
      ~f)
;;

let%expect_test "assemble(flatten(sexp)) = sexp" =
  forall_sexps [%here] ~f:(fun sexp ->
    [%test_eq: Sexp.t] sexp (Parts.assemble (Parts.flatten sexp)));
  [%expect {| |}]
;;

let%expect_test "flatten does the same as Path.get" =
  forall_sexps [%here] ~f:(fun sexp ->
    let parts = Parts.flatten sexp in
    List.iter parts ~f:(fun (path, sexp_part) ->
      match Sexplib.Path.get ~path sexp with
      | gotten_part -> [%test_eq: Sexp.t] sexp_part gotten_part
      | exception exn -> Exn.reraisef exn !"invalid path %{Parts.Path}" path ()));
  [%expect {| |}]
;;

let%expect_test "Path.get respects assembled sexps" =
  forall_sexps [%here] ~f:(fun sexp ->
    let parts = Parts.flatten sexp in
    let assembled = Parts.assemble parts in
    List.iter parts ~f:(fun (path, sexp_part) ->
      match Sexplib.Path.get ~path assembled with
      | gotten_part -> [%test_eq: Sexp.t] sexp_part gotten_part
      | exception exn -> Exn.reraisef exn !"invalid path %{Parts.Path}" path ()));
  [%expect {| |}]
;;

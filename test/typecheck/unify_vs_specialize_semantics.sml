(* t-compile: --dump-typecheck *)

(* This test is a classic example of the Ocaml/ML difference.
* In ML this should fail. In Ocaml, this will suceed.
*)
val y = 4: 'a

(* t-scan: int: typecheck*)

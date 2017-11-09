(* t-compile: --dump-typecheck *)
(* We require that the type environment has two items
* in it. *)

val x = 1
val x = 2

(* t-scan-times-2: int : typecheck *)

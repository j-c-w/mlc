(* t-compile: --dump-typecheck *)

val y = []
val z = [[1], y, [1]]
val x = [y, [1]]

(* t-scan: int: typecheck *)
(* t-scan: int list list : typecheck *)

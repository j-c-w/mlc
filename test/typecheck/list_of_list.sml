(* t-compile: --dump-typecheck *)

val y = []
val z = [[1], y, [1]]
val x = [y, [1]]

(* t-scan-times-2: int: typecheck *)
(* t-scan-times-2: int list list : typecheck *)

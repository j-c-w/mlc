(* t-compile: --dump-typecheck *)

val (x, (a, (b, c), d)) = (1, (1, (2, 3), 3))
val z = x

(* t-scan-times-6: int: typecheck *)

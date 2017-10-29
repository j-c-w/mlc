(* t-compile: --dump-typecheck *)

val x = 4
val y = x

(* scan-times-2: int: typecheck *)

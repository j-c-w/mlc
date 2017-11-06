(* t-compile: --dump-typecheck *)

val x = [1, 2, 3, 4]
val y = ["Hi", "there"]
val z = 4 :: x

(* t-scan: int list: typecheck *)
(* t-scan: string list: typecheck *)

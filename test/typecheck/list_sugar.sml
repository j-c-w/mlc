(* t-compile: --dump-typecheck *)

val x = [1, 2, 3, 4]
val y = ["Hi", "there"]
val z = 4 :: x

(* t-scan-times-2: int list: typecheck *)
(* t-scan-times-1: string list: typecheck *)

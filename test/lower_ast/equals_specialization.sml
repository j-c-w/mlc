(* t-compile: --dump-tir *)

val y = "A"
val z = "B"
(* t-scan-times-1: =s: lower_ast *)
val x = if (y = z) then print("Equal") else print("Not Equal")

val y = 1
val z = 2
(* t-scan-times-1 : =i: lower_ast *)
val x = if (y = z) then () else ()

(* t-scan-times-1: =gen: lower_ast *)
fun f x y = x = y

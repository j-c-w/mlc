(* t-compile: *)
(* t-run: pass *)

fun f x = 2 * x + 1
fun g x = x

val x = (g f) 1

val _ = if (x = 3) then print(g "pass") else print(g "fail")

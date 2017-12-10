(* t-compile: *)
(* t-run: pass *)

fun f g x = g x
fun g y = y + 1

val x = f g 2

val _ = if (x = 3) then print("pass") else print("fail")

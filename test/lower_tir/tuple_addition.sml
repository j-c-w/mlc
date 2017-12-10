(* t-compile: *)
(* t-run: pass *)

fun add (x, y) (a, b) = (x + a, y + b)

val x = add (1, 2) (3, 4)

val _ = if x = (4, 6) then print "pass" else print "fail"

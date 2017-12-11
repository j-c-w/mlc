(* t-compile: *)
(* t-run: 3.0pass *)

val x = [1]
val y = x @ [2]
val z = y @ [3]

val _ = print(Real.toString(Real.fromInt(length(z))))
val _ = if (z = [1, 2, 3]) then print "pass" else print "fail"


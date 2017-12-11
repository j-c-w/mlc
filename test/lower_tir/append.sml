(* t-compile: *)
(* t-run: pass *)

val x = [1] @ [2] @ [3]

val _ = if (x = [1, 2, 3]) then print "pass" else print "fail"

(* t-compile: *)
(* t-run: pass *)

val x = if (true andalso 1 <> 2) orelse false then 1 else 0

val _ = if (x = 1) then print "pass" else print "fail"

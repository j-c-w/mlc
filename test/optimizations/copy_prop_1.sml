(* t-compile: --dump-copy-prop --verify-all *)
(* t-run: pass *)
(* t-scan: Number of copy propagations is 3: copy_prop *)

val x = 10
val y = x
val z = y
val a = z
val result = a + z

val _ = if (result = 20) then print "pass" else print "fail"

(* t-compile: --f-tce --dump-tce --verify-all *)
(* t-run: pass *)
(* t-scan: Number of functions replaced = 1: tail_elim *)

fun f 0 = 0
  | f x = f (x - 1)

val _ = if (f 10000 = 0) then print "pass" else print "fail"

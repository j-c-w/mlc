(* t-compile: *)
(* It is important to check that the inliner does not re-inline this. *)
(* t-compile: --f-t-inline *)
(* t-run: pass *)

exception a
exception b

fun f 0 = raise a
  | f 1 = raise b

val sum =
  (f 0 handle a => 1) + (f 1 handle b => 2)

val _ =
  if sum = 3 then print "pass" else print "fail"

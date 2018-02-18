(* t-compile: *)
(* t-run: pass *)

exception ex
exception notex

val x =
  ((raise ex) handle notex => 1) handle ex => 2

val _ =
  if x = 2 then print "pass" else print "fail"

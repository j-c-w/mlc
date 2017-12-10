(* t-compile: *)
(* t-run: 1234 *)

(* These two are the easy cases.  Harder cases specified below.
*  This seems to be unspecified in the ML spec, so we just take
*  a 'senesible' order of evaluation for these arguments.  *)
fun f x y = x
fun g (x, y) = y

val _ = f (print "1") (print "2")
val _ = g (print "3", print "4")

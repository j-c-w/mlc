(* t-compile: --f-tce --f-t-inline *)
(* Regression: This used to fail.  'f' would be converted  into a loop,
* and the same label would be re-used upon inlining it twice.  *)

fun f x = f x

fun g x = g (f x, f x)

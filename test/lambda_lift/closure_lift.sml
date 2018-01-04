(* t-compile: --verify-all *)
(* Regression: The lambda lifter was not putting the application identifier
* for the closure in the right place.  *)

fun id y = y
fun f x = (fn _ => id x)

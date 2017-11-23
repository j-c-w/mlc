(* t-compile: *)
(* Regression: lower_program did not consider that higher order functions
* may have multiple structures for the same type.  *)

(* These have the same type, but different structure.  *)
fun f x y = 1
fun f x = fn y => 1


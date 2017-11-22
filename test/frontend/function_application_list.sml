(* t-compile: *)

fun f x = x
val x =
  (f [1]; f(1, 2))

(* Regression: Lists were not treated as simple identifiers in the grammar. *)

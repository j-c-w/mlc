(* t-compile: *)
(* Regression: Tuple types were not properly grouped.  *)

fun f x: (real * real * real) * (real * real *  real) = x

val _ = f ((1.0, 1.0, 1.0), (1.0, 1.0, 1.0))

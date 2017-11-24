(* t-compile: --dump-typecheck *)
(* Regression: There was a bug in the specialization of int types and equaltiy
* types that led to x and y being specialized to reals.  *)

fun f x y = (x = y) orelse (x + y = y)

(* t-scan: int: typecheck *)

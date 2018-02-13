(* t-compile: *)
(* Regression: We were not correctly keeping track of equality types within
 * data types.  *)

datatype t = A of int | B of string * t list

fun match_rec (B(_,a)) = a = a
  | match_rec n = n = n

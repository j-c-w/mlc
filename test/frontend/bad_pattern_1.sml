(* t-compile: *)
(* t-fail *)
(* Regression. Pattern matches like this are only allowed to take
* place nested within parens.  *)

fun f x :: xs = 10

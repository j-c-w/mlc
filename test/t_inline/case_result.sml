(* t-compile: -O --verify-all *)
(* Regression: This test case is a case of a case expression generating
*  a function as a result.  *)

fun app f x = f x
fun id x = x

fun f p =
  (case (app, id) of
    (x, y) => x y) 0

val _ = f 0

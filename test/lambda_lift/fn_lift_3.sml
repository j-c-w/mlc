(* t-compile: --dump-lambda-lift *)

val x = 1

fun f y = fn z => (x, y)

val _ = if (f 3 2) = (1, 3) then print "pass" else print "fail"

(* We check that the function returned is properly applied.
* To do this, look for a function application in the result.  *)
(* t-scan: \(TopLevel.#[a-z]*#\) *\(@[a-z]*#[a-z]+\): lambda_lift *)

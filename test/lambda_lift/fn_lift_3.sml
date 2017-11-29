(* t-compile: --dump-lambda-lift *)

val x = 1

fun f y = fn z => (x, y)

(* We check that the function returned is properly applied.
* To do this, look for a function application in the result.  *)
(* t-scan: \(TopLevel.#[a-z]*#\) *\(\(@[a-z]*#[a-z]+\)\): lambda_lift *)

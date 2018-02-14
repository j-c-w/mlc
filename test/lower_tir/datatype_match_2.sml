(* t-compile: *)
(* t-run: pass *)
(* Regression: 'A' was treated as a variable name rather than a pattern
* match.  *)

datatype a = A
           | B of string


fun f A = "fail"
  | f (B(y)) = "pass"

val _ = print(f(B("")))

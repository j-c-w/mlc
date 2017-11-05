(* t-compile: *)
(* Regression: Infix operations were not assigned application types.
* This does not need a scan, as the LowerAST pass checks for it. *)

fun member [] _ = false
  | member (x :: xs) y = (x = y) orelse member xs y

(* We had the same problems with unary operations. *)
fun unop x = ~(x)


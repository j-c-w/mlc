(* t-compile: --dump-typecheck *)

fun loop z = loop z

fun f x =
let
  val (a, b, c, d) = f 1
in
  loop a
end

(* Regression: Some functions require non-traditional ways of determining the
* type. *)
(* t-scan: int *-> *\(: typecheck *)

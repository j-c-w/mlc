(* t-compile: *)

fun f x y = x + Real.fromInt(y)
(* t-scan: \(real ->  \(int -> real\) : typecheck *)

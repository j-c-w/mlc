(* t-compile: --dump-typecheck *)

fun f x y = x + Real.fromInt(y)
(* t-scan: \(real ->  \(int -> real\) : typecheck *)

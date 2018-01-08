(* t-compile: --verify-all *)
(* Minor adaptations to fit this subset.  Taken from:
* https://www.cs.cmu.edu/~rwh/introsml/samplecode/typinf.sml *)

(* Type Inference *)

val I = fn x=>x

fun I(x:'a):'a = x

val I = fn x=>x

fun I(x) = x

val J = I I

fun J x = I I x

val l = nil @ nil

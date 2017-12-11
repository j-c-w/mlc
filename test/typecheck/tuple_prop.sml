(* t-compile: --dump-typecheck *)

fun f(x,y) = (x * y, 2.0*x)
fun g(x,y) = (x ^ y, x <= y)

(* t-scan: \(real \* real\) *-> *\(real \* real\) : typecheck  *)
(* Also look for the function application <= to be typed as string * string *)
(* t-scan: \(string \* string\) *-> *bool : typecheck *)

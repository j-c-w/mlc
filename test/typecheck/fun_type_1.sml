(* t-compile: --dump-typecheck *)

fun f x = x
fun g x = (f x) + 1

(* Expect to find this once in the type of g, and another time
* in the type of the application of f to x.  *)
(* t-scan-times-2 : int -> int : typecheck *)

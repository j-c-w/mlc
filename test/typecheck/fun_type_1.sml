(* t-compile: --dump-typecheck *)

fun f x = x
fun g x = (f x) + 1

(* t-scan-times-1 : int -> int : typecheck *)

(* t-compile: --dump-typecheck *)

fun f x y = x = y

(* t-scan-2: ''[a-z]+ : typecheck *)

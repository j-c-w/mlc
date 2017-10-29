(* t-compile: --dump-typecheck *)

fun f x x = x + 1

(* We expect the first variable to be polymorphic *)
(* t-scan: '\$[a-z]+ : typecheck *)

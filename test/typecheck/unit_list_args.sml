(* t-compile: --dump-typecheck *)

fun f () = 1

fun f [] = 1

(* t-scan-times-1: unit *\-> *int: typecheck *)
(* t-scan-times-1: '\$[a-z]+ *list : typecheck *)

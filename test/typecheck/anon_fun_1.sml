(* t-compile: --dump-typecheck *)

val x = fn y => y

(* t-scan: '\$[a-z]+ -> '\$[a-z]+ : typecheck *)

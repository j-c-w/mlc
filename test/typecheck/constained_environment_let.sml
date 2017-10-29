(* t-compile: --dump-typecheck *)

val x = let val y = 1 in 2.0 end

(* t-scan-not: int:typecheck *)

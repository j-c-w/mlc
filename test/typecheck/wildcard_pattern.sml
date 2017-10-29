(* t-compile: --dump-typecheck *)

fun f true = 1
  | f _ = 0

(* t-scan: bool -> int : typecheck *)

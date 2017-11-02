(* t-compile: --dump-typecheck *)

fun m (1 :: xs) = xs

(* t-scan: int list -> int list: typecheck *)

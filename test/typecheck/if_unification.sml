(* t-compile: --dump-typecheck *)

val x = if true then [] else [1]

(* t-scan: int list: typecheck *)

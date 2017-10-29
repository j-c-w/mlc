(* t-compile: --dump-typecheck *)

val a = ([] : 'a list)
val x = if true then a else [1]

(* t-scan: 'a list : typecheck *)

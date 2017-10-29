(* t-compile: --dump-typecheck *)

fun f (x :: xs) = 0
  | f ([]) = 1

(* t-scan: \('\$[a-z]+ list\) *-> int: typecheck *)

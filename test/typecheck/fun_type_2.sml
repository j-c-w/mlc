(* t-compile: --dump-typecheck *)

fun g 0 x = 1
  | g 1 2 = 1
  | g y z = 1

(* t-scan: int ->  \(int -> int\) : typecheck *)

(* t-compile: --dump-typecheck *)
(* t-fail *)

fun last ([x], 0)= x
  | last (x :: xs, 1) = last(xs)

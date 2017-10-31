(* t-compile: --dump-typecheck *)

fun f x y = x < y

fun g x y =
  if (x < y) then
    x ^ y
  else
    y ^ x

fun cmp_rl x (y : real) = x <= y

fun cmp_str x y = x >= (y: string)

(* t-scan-times-6: string: typecheck *)
(* t-scan-times-2: int: typecheck *)

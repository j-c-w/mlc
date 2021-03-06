(* t-compile: --dump-typecheck *)

fun f x y = x < y

fun g x y =
  if (x < y) then
    x ^ y
  else
    y ^ x

fun cmp_rl x (y : real) = x <= y

fun cmp_str x y = x >= (y: string)

(* t-scan-times-1: g. *\(string -> *\(string -> string\): typecheck *)
(* t-scan-times-1: cmp_str. *\(string -> *\(string -> bool\): typecheck *)
(* t-scan-times-1: f. *\(int -> *\(int -> bool\): typecheck *)

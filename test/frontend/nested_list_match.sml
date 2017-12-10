(* t-compile: --dump-typecheck *)
(* Regression: This kind of match was failing to parse correctly.  *)

fun f ((x :: xs) :: ls) = x

fun g ((x :: xs): string list) = x

fun h (((x :: xs): int list) :: ls) = ls

(* t-scan: int list list -> int list list : typecheck *)
(* t-scan: string list -> string: typecheck *)

(* t-compile: --f-t-inline --dump-t-inline *)

fun f x: int = x
fun g x = f x

(* t-scan: Number of inlines = 1: t_inline *)

(* t-compile: *)

val a = ([] : 'a list)
val x = if true then a else a

(* We require that x is assigned a fresh type,
* so that the type of 'a' does not propagate *)
(* t-scan-1: 'a list : typecheck *)

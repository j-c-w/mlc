(* t-compile: --f-t-inline *)
(* This was failing due to poor renaming choices when inlining the pattern
 * match.  *)

exception a of string

fun f (a(value)) = value

val _ = f(a("Hi"))

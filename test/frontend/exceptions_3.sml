(* t-compile: *)
(* t-run: pass *)

exception a of string
exception b of real

fun f (a(value): exn) = value
  | f (b(value): exn) = Real.toString(value)

val _ = if f(b(1.0)) = "1.0" then print "pass" else print "fail"

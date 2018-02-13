(* t-compile: *)
(* t-run: pass *)

datatype k = A of int
           | B of string * string * string

fun f (A(1)) = 1
  | f (A(x)) = 2
  | f (B(_, _, _)) = 3

val _ = if f(A(1)) = 1 then print "pass" else print "fail"

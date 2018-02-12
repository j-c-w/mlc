(* t-compile: *)
(* t-run: 2.0 *)

exception a of int

fun f x = (raise a 1) handle x => 2

val _ = print(Real.toString(Real.fromInt(f 2)))

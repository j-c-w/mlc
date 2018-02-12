(* t-compile: *)

exception a of string * exn

fun f (a("hi", a("hi", _))) = 1

(* t-compile: *)
(* t-fail *)

exception a of int;

val _ = raise a(1) handle _ => 1

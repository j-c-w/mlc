(* t-compile: *)
(* Taken from SMLNJ testsuite. *)

fun f (x: 'a) = x : 'a
fun g (y: 'a) = (f y):'a;

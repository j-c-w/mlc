(* t-compile: *)
(* t-fail *)

(* We expect the compiler not to crash if either
* of these constansts is too big *)
val x = 100000000000000000000000000
val y = 1e30000000000000000000

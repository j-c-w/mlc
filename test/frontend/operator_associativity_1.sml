(* t-compile: --dump-ast *)
(* Regression: No operator associativity. *)

fun f x y z = x < y andalso z

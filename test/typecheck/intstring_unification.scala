(* t-compile: --dump-ast *)
(* Regression: Unification did not work properly for int string types. *)

fun f x y z = x = y andalso z < y

(* t-compile: --dump-ast *)
(* Regression: When chaing the fronend to accept function applications
* with the right arity, we ran into issues with function association.  *)

fun id x = x

fun f x = (f (id x))

(* t-scan: \(id\) *\(x\): ast *)

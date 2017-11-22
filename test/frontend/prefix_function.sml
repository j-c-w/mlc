(* t-compile: --dump-ast *)

fun f x = x 1 + 1
fun g x = ~ x + x

(* Regression. We expect the ~ and 'x' function to apply to the first
* argument only. *)
(* t-scan: ~ \(x\): ast *)
(* t-scan: \(x\)\(1\): ast *)

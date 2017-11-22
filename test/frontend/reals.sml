(* t-compile: --dump-ast *)

val a = 1.1
val b = 1.2e1
val c = 2e1
val d = ~1.2e~1
val e = 1E1

val f = 0001.0001e~0001
val g =
  100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e~99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999

val h =
  999999999999999999999999999999999999e9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999

(* Expect to see these in Java big decimal notation.  *)
val i = 0.111e2
(* t-scan: 11.1: ast *)
val j = 7.7e~1
(* t-sca: 0.77: ast *)

(* t-compile: --dump-peephole *)

val x = 1 + 1

(* t-scan-not: valueOf.*\n.*Value : peephole *)

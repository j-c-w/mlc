(* t-compile: --dump-ast *)

val x = 4 + 5
val y = x + 10

(* t-scan: val x = 4 \+ 5: ast *)
(* t-scan: val y = x \+ 10: ast *)

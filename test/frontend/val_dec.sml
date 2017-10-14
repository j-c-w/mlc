(* t-compile: --dump-ast *)

val x = 4 + 5
val y = x + 10

(* t-scan: val x = 4 \+ 5: val_dec.sml.0.ast *)
(* t-scan: val y = x \+ 10: val_dec.sml.0.ast *)

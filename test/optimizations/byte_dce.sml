(* t-compile: --f-byte-dce --dump-peephole *)
(* Note we dump the peephole pass rather than the byte DCE pass.
 * This is to avoid issues with the old code still being present in the DCE
 * dump file.  *)

val x = 1
val y = 2

(* t-scan-not: _null: peephole *)

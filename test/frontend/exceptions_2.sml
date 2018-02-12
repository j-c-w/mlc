(* t-compile: *)
(* There was a bug where the copy propagator was copy propagating
* the try variable.  This should not happen obviously.  *)
(* t-compile: --f-copy-prop --verify-all *)
(* t-run: pass *)

exception ex

val res = (raise ex) ^ "fail" handle ex => "pass"
val _ = print(res)

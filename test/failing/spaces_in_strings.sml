(* t-compile: --dump-ast *)

(* The spaces on identifiers are currently stripped out.  *)
val x = "Hello world"

(* should have: t-scan: Hello world: ast *)

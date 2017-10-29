(* t-compile: --dump-ast *)
(* Regression: Unit identifiers in patters were being treated
* as empty tuples *)

fun f () = 1

(* t-scan: ASTUnitIdent : ast *)

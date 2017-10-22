(* t-compile: --dump-ast *)

val x = true andalso false
val y = true orelse false

(* t-scan-1: ASTExpAnd: ast*)
(* t-scan-1: ASTExpOr: ast*)

(* t-compile: --dump-ast *)

(* We expect 'x' to be identified as a short ID rather
* than a long ID *)

val x = 1
val y = 1 + x

(* t-scan-not: ASTLongIdent: ast *)

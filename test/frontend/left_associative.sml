(* t-compile: --dump-ast *)
(* Regression: Things were not left associated. *)

fun f x y z = x * y * z

(* t-scan: ASTExpInfixApp\(ASTTimesIdent\(\),ASTExpInfixApp\(ASTTimesIdent\(\),ASTExpIdent\(ASTIdentVar\(x\)\),ASTExpIdent\(ASTIdentVar\(y\) : ast *)

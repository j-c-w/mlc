(* t-compile: --dump-ast *)
(* Regression: No operator associativity. *)

fun f a b c d = a * b + c * d

(* t-scan: ASTExpInfixApp\(ASTPlusIdent\(\),ASTExpInfixApp\(ASTTimesIdent: ast *)

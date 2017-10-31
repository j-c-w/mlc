(* t-compile: --dump-ast *)
(* Regression: No operator associativity. *)

fun f x y z = 6 + x = z div 7

(* t-scan: ASTExpInfixApp\(ASTEqIdent\(\),ASTExpInfixApp\(ASTPlusIdent: ast *)

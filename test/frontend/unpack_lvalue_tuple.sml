(* t-compile: --dump-ast *)
(* Regression: We were flattening tuples of identifiers on the LHS of
 * expressions. *)

val (x, (c, d)) = (1, (3, 3))

(* t-scan: ASTIdentTuple\(List\(ASTIdentVar\(x\), ASTIdentTuple:ast *)

(* t-compile: --dump-ast *)

val x = (if true then [] else [], 2)

(* This is due to a bug with the association of tuples. *)
(* t-scan-1: ASTExpTuple\(List\(ASTExpIfThenElse : ast *)

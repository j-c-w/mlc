(* t-compile: --dump-ast *)
(* t-run: pass *)
(* Regression: We were flattening tuples of identifiers on the LHS of
 * expressions. *)

val (x, (c, d)) = (1, (2, 3))

val _ = if (x = 1) andalso c = 2 andalso d = 3 then
          print "pass"
        else
          print "fail"

(* t-scan: ASTIdentTuple\(List\(ASTIdentVar\(x\), ASTIdentTuple:ast *)

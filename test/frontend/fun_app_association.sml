(* t-compile: --dump-ast *)
(* Regression: not associating fuction applications correctly *)

fun g x y = x
fun f x = g 2 1

(* Basically, there was a bug where in the above we would associate
*  it as (g ((2) (1))), which obviuosly makes no sense. This scan
*  checks that we associate it as ((g 2) 1)
*)
(* t-scan: ASTExpFunApp\(ASTExpFunApp\(ASTExpIdent\(ASTIdentVar\(g\)\): ast *)

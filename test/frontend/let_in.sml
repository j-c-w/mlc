(* t-compile: --dump-ast *)

val x = let
  val a = 10;
in
  1
end

(* t-scan: ASTExpLetIn: ast*)

(* t-compile: --dump-ast *)

val x = case true of 
            true => 1
           | false => 0

(* Require that we pick these up as booleans rather
* than strings. *)
(* t-scan-times-2: ASTConstTrue\(\): simple_case.sml.0.ast *)
(* t-scan-times-1: ASTConstFalse\(\): simple_case.sml.0.ast *)

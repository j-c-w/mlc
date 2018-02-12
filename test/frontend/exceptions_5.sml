(* t-compile: --dump-ast *)
(* t-scan-times-4: ASTPatConstructor: ast *)

exception a of int * int * int
exception b of string * int

val _ = 1 handle a(1, 1, 1) => 4
               | a(_, _, _) => 3
               | b("Hi", 1) => 2
               | b(_, _) => 1

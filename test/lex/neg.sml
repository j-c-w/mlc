(* t-compile: --dump-lex *)

val x = ~ 1.0
val x = ~1.0
val y = ~ 1
val y = ~1

val z = 7.8

(* t-scan-not: LexNeg : lex *)
(* t-scan-times-2: LexIntLiteral\(-1\) : lex *)
(* t-scan-times-2: LexFloatLiteral\(-1.0\) : lex *)
(* t-scan-times-1: LexFloatLiteral\(7.8\) : lex *)

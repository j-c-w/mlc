(* t-compile: --dump-lex *)
(* t-run: Hello world *)

(* t-scan: LexCharLiteral\(' '\) : lex *)
val k = #" "
(* t-scan: LexStringLiteral\("Hello world"\): lex *)
val x = "Hello world"
val _ = print x

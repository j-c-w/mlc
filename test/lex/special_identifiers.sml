(* t-compile: --dump-lex *)

fun f _ _ = 1

fun g () () () () () () () () () = 2

(* t-scan-times-2: LexUnderscore: lex *)
(* t-scan-times-9: LexUnit: lex *)

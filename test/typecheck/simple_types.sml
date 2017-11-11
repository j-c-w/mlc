(* t-compile: --dump-typecheck *)

val x = [4, 4]
val y = "HELLO"
val z = 1.0
val a = #"c"

(* t-scan-times-1: string:typecheck *)
(* t-scan-times-1: real :typecheck*)
(* t-scan-times-1: char:typecheck *)
(* t-scan: int list:typecheck *)

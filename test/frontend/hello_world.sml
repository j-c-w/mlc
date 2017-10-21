(* t-compile: --dump-ast *)

val str = "Hello"
val str2 = "World"
fun convert x = print(str ^ " " ^ str2)

(* We should ensure that the strings are interpreted as whole
* strings rather than their individual characters. *)

(* t-scan-times-1: ASTConstString\(Hello\) : ast*)
(* t-scan-times-1: ASTConstString\(Hello\) : ast*)

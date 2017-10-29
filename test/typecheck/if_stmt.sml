(* t-compile: --dump-typecheck *)

val x = if false then 1.0 else 0.0

(* t-scan-times-1: real: typecheck*)

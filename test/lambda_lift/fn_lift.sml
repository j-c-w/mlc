(* t-compile: --dump-lambda-lift *)

val x = (fn x => x) 1

(* t-scan-not: fn : lambda_lift *)

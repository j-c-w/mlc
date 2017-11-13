(* t-compile: --dump-lambda-lift *)

val q = true
val z = if q then (fn (x, y) => x + y) else (fn (x, y) => x + 1.0 + y)
val y = fn (x, y) => x + y

(* t-scan-not: fn : lambda_lift *)

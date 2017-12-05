(* t-compile: --dump-lambda-lift --dump-tir *)

val x =
  let
    val t = 1
    fun y x = x + t
  in
    y
  end

(* t-scan-times-3: val : lambda_lift *)

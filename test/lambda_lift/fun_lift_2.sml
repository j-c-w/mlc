(* t-compile: --dump-lambda-lift --dump-tir *)
(* t-run: pass *)
(* This has been annotated because it was a _really_ nasty bug
* (many many days).  Check that all variables are assigned to
* on all paths! *)

val x =
  let
    val t = 1
    fun y x = x + t
  in
    y
  end

val _ = if x(1) = 2 then print "pass" else print "fail"

(* t-scan-times-3: val : lambda_lift *)

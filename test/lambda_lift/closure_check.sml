(* t-compile: --dump-lambda-lift *)

val x =
  let 
    val t = 1
    fun y x = t + x
  in
    (1; y)
  end


(* We check that the function has two arguments, one of which
* has been generated. *)
(* t-scan-times-1: fun @[a-z]+#y\s*\(%[a-z]+\)\s*@[a-z]+#x : lambda_lift *)

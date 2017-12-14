(* t-compile: --dump-lambda-lift --dump-tir *)
(* t-run: pass *)

val x =
  let fun y x = x
  in
    y
  end

(* Note that this application, while disallowed by full ML compilers,
*  is allowed here since we do not have refs.  *)
val _ = if (x true) then print "pass" else print "fail"

(* We check that there has been a new top level identifier introduced
* by the lambda lift pass. *)

(* Duplicated since we have one for the type environment.  *)
(* t-scan-times-3: TopLevel_ : lower_ast *)
(* t-scan-times-6: TopLevel_ : lambda_lift *)

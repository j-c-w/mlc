(* t-compile: --dump-lambda-lift --dump-tir *)

val x =
  let fun y x = x
  in
    y
  end

(* We check that there has been a new top level identifier introduced
* by the lambda lift pass. *)

(* Duplicated since we have one for the type environment.  *)
(* t-scan-times-2: TopLevel_ : lower_ast *)
(* t-scan-times-5: TopLevel_ : lambda_lift *)

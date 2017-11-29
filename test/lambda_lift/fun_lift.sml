(* t-compile: --dump-lambda-lift --dump-lower-ast *)

val x =
  let fun y x = x
  in
    y
  end

(* We expect the function to leave the 'let' and the 'in'
* right next to each other *)
(* t-scan-times-1: let\s*in : lambda_lift *)

(* We check that there has been a new top level identifier introduced
* by the lambda lift pass. *)

(* Duplicated since we have one for the type environment.  *)
(* t-scan-times-2: TopLevel_ : lower_ast *)
(* t-scan-times-4: TopLevel_ : lambda_lift *)

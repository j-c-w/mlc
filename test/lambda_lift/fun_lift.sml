(* t-compile: --dump-lambda-lift *)

val x =
  let fun y x = x
  in
    y
  end

(* We expect the function to leave the 'let' and the 'in'
* right next to each other *)
(* t-scan-times-1: let\s*in : lambda_lift *)

(* t-compile: --dump-lower-program *)

fun f x =  
  let val y = 1
  in y + x end

(* t-scan-not: let : lower_program *)
(* t-scan-times-4: Assign: lower_program *)

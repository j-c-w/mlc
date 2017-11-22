(* t-compile: --dump-lower-program *)

val x = case (4, 5) of 
   (x, _) => 1
 | (_, y) => 3

(* Also scan for a tuple extraction *)
(* t-scan: \._0 : lower_program *)
(* t-scan: \._1 : lower_program *)

(* t-compile: --dump-lower-program *)

val x = case (4, 5) of 
   (x, _) => 1
 | (_, y) => 3

(* We simply check for the loads from the arguement tuple
* here rather than specifying 2 as there (in currently implemtation)
* 4 loads done. These are going to (ideally) be removed by a future
* pass. *)
(* t-scan: Argument_[0-9] : lower_program *)
(* Also scan for a tuple extraction *)
(* t-scan: \._0 : lower_program *)
(* t-scan: \._1 : lower_program *)

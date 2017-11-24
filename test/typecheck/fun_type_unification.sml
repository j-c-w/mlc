(* t-compile: --run-verify-unifiers *)
(* Regression:  This used to insert mutually referential types
* into the unifier. *)

fun f [] = true

fun g (x, y) =
      if f y then g (x, y)
      else g (x, x::y)

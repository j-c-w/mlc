(* t-compile: *)
(* t-fail *)

(* This should pass.  *)
fun f x: string = (x + 1; "Hello")

(* This is an additional test for the easy mistake to make when fixing this. *)
fun f (x: int): string = (x + 1; "Hello")

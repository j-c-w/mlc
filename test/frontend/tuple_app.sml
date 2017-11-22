(* t-compile: *)

(* Regression: The + was incorrectly associated so this became
* x ((y, y) + 1) *)
fun f x y = x (y, y) + 1

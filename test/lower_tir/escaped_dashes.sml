(* t-compile: *)
(* Regression: The LowerString method did not escape characters like ' *)

val x = "'"
val y = "\n\t\t"

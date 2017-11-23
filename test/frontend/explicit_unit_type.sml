(* t-compile: --dump-ast *)
(* Regression: The frontend did not understand an explicit unit type. *)

fun foreach (f: 'a -> unit) =()

(* t-scan: ASTUnitType *)

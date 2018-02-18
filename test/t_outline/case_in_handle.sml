(* t-compile: *)
(* Regression: The t_outline pass was constructing the wrong environment
* hierarchy for this structure.  Lambda lift was failing as a result.  *)

exception F of unit;

fun f M =
  true handle F _ =>
    case M of (_,x) => x = []

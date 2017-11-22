(* t-compile: *)
(* Regression: This was crashing the lower program pass due to a disconnect
* between tuples being eliminated as types but not as patterns. *)

fun tup_match(((((x1, y1))))) = (x1, y1)

fun tup_ret(x) = ((((x, x))))

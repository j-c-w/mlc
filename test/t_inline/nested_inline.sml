(* t-compile: *)
(* Regression: the inliner was not properly typing all function applications.
 * This has been 'fixed' by not inling anything we have not costed.  *)

datatype t = T of int * t list

fun v x =
let
  fun vs (t::r) = v t
in
  case x of
      (T(_, l)) => vs l
end


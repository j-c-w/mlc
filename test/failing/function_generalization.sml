(* t-compile: *)
(* This should fail.  It has to do with the implicit assumption of the
* typechecker that every typevariable is qualifiable when a function typing
* is finished (when in fact, in cases like this, we get a type variable that
* has not yet been finished being specialized so cannot be qualified yet.  *)
(* Regression: Type variables were generalized before they should have been
 * in functions.  *)

fun g y =
  let
      fun h z = g y
  in h end

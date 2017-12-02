(* t-compile: --verify-all *)
(* Regression: The overriden let binding was not overwriting the expression
* if it needed to be overwritten.  *)

val x =
  let 
    val t = let
    in
      fn x => x
    end
  in
    t
  end

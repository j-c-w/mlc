(* t-compile: --run-verify-unifiers *)
(* Regression: This case broken when I introduced a change to the unifier
* to fix a dual case.  *)

fun f(y) =
    if y < y then ()
    else
      let fun swap 0 = [y]
        | swap 1 =
          if y>y then []
                else [y]
      in () end


(* t-compile: --verify-all *)
(* Regression: This used to kill the lambda lifter due to the reference
* to  'g' from within 'h'. *)

fun f z x =
  let fun g y =
        let fun h z =
                z + g y
        in x + h 1 end
  in g end

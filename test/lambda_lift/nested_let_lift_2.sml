(* t-compile: --verify-all *)
(* Regression: This used to kill the lambda lifter due to the reference
* to  'g' from within 'h'. *)

fun f x =
  let fun g y =
        let fun h z =
                g y
        in h 1 end
  in g end

(* t-compile: --verify-all *)
(* Regression: Changing names during a walk did not update the names
* in type envoronments without uses of that variable.
*
* This was problematic in later passes that assumed variables
* exist everywhere that they /could/ be used.  *)

fun f(y) =
      let fun swap [x] = y
            | swap _ = ()
      in swap([1]) end

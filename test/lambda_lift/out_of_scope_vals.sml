(* t-compile: *)
(* Regression.  The lambda lifter is not identifying closures entirely
 * correctly.  It was identifying values not in scope (here 'son') to be
 * passed as parameters.  This has been 'fixed' by ensuring that only paramters
 * in scope of a function may be used.  This may or may not have consequences.
 *)

datatype term =
  Term of string * term list
fun top t =
  let fun inner M =
  let
    fun g() =
      let fun tryrec (son::rest) =
      let
        fun h (son') = (inner son')
      in
        h(son)
      end
      fun i (Term(f, sons)) = tryrec sons
      in i(M)
      end
  in g() end
  in inner
  end

(* t-compile: --run-lower-program-verify *)
(* Regression: Since we don't bother keeping the types for
* underscore identifiers, the tuple generator was having a hard
* time keeping track of the type.  This was resolved by giving
* the _ identifier a generic type (after all , it is not used, so
* allocating it to an object does not hurt). *)

val x =
  let
    val z = let
      val (x, _, _, _) = (1, "String", (), ((), 3.0))
    in
      x
    end
  in
    z
  end

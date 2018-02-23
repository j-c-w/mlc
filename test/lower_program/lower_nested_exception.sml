(* t-compile: *)
(* Regression: The change_names_walk in lower program was
 * not putting types where they needed to go (e.g. the top level)
 * if they were encountered at inner levels first.  *)

fun f () =
let
  exception E of int
  fun l((E x) :: xs) = ()
    | l(_ :: xs) = l xs
in l
end

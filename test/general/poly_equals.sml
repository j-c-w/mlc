(* t-compile: *)
(* t-compile: -O *)
(* Extracted from PolyML test 169.  *)

fun foldr f n [] = n
|   foldr f n (hd :: tl) = f (hd, foldr f n tl)

fun eq _ a b = a = b

fun less_eq_list A_ (x :: xs) (y :: ys) =
  (if eq A_ x y then less_eq_list A_ xs ys else less_eq_list A_ (x :: xs) ys)

(* less_eq_list always terminates by raising Match so
   the andalso is dead code. *)
fun less_list A_ xs ys =
  less_eq_list A_ xs ys andalso not (less_eq_list A_ ys xs)


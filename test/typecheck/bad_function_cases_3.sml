(* t-compile: *)
(* t-fail *)

fun bad_match (x :: xs) = x
  | bad_match (x, y) = (x, y)

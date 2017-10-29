(* t-compile: *)
(* t-fail *)

val a = ([] : 'a list)
val x = if a then [] else [1]

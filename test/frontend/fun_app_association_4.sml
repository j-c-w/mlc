(* t-compile: *)

fun g x y z a b c = c

fun f x y = g x (Real.fromInt(y)) y y y y

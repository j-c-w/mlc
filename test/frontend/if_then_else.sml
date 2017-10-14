(* t-compile: *)

val x = if true then true else false
val y = if true then false else if false then true else if false then true else
  false
val z = if true then if true then true else false else false

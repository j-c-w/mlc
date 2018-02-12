(* t-compile: *)
(* t-fail *)

fun f x =
let exception c of x
in 1
end

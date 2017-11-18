(* t-compile: --dump-lower-program *)

fun f x y z = x + y + z

(* t-scan-times-3: Argument_[0-9] : lower_program *)

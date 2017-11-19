(* t-compile: --dump-typecheck *)

fun listMatch [] [] [] () () ((), ()) ([[]], []) = 1

fun unitMatch () (((()))) [(), (), ()] = 1

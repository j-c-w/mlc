(* t-compile: --dump-typecheck *)

fun f x = let
  fun h y x = y + x
in
  h x x
end

(* t-scan: int -> int: typecheck *)

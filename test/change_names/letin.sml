(* t-compile: --dump-change-names *)

val x =
  let
      val x = 10
      val x = 1 + x
   in
     x + x
  end

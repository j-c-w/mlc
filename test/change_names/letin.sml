(* t-compile: --dump-change-names *)
(* t-run: pass *)

val x =
  let
      val x = 10
      val x = 1 + x
   in
     x + x
  end

val _ = if (x = 22) then print "pass" else print "fail"

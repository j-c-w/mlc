(* t-compile: *)
(* t-run: Works *)

val x = 1 + 4
val x = x * 20
val x = x - 10
val x = x div 8

val y = 1.0 + 4.0
val y = y * 0.5
val y = y - ~1.5
val y = 2.0 / y

val _ = if x = 11 andalso y <= 0.51 andalso y > 0.49 then
  print "Works" else print "Buggy"

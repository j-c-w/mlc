(* t-compile: *)
(* t-run: pass *)

fun equals x y = x = y

val a = "a" = "a"
val b = 1 = 1
val c = #"c" = #"c"

val d = equals "a" "a"
val e = equals 2 2
val f = equals #"e" #"e"
val g = equals (1, "string") (1, "string")
val h = equals (((1, 2), #"a"), 1)
               (((1, 2), #"a"), 1)
val i = [] = []

val _ =
  if (a andalso b andalso c andalso d andalso e andalso
      f andalso g andalso h andalso i) then
    print "pass"
  else
    print "fail"

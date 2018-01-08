(* t-compile: --verify-all *)
(* Adapted from https://www.cs.cmu.edu/~rwh/introsml/samplecode/clauses.sml *)
(* Some minor changes made to make this compile.  *)


fun abs (x) = if x < 0.0 then ~x else x

val dist = fn (x:real, y:real) => Math.sqrt (x*x + y*y)

fun dist (x:real, y:real):real = Math.sqrt (x*x + y*y)

fun dist2 (x:real, y:real):real*real = (Math.sqrt (x*x+y*y), abs(x-y))

val recip = fn 0 => 0 | n:int => 1 div n

fun recip 0 = 0
  | recip (n:int) = 1 div n

fun is_alpha (c:char) =
    (#"a" <= c andalso c <= #"z") orelse (#"A" <= c andalso c <= #"Z")

fun recip 0 = 0
  | recip (n:int) = 1 div n

fun is_numeric #"0" = true
  | is_numeric #"1" = true
  | is_numeric #"2" = true
  | is_numeric #"3" = true
  | is_numeric #"4" = true
  | is_numeric #"5" = true
  | is_numeric #"6" = true
  | is_numeric #"7" = true
  | is_numeric #"8" = true
  | is_numeric #"9" = true

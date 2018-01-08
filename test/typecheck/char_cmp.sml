(* t-compile: --verify-all *)
(* t-run: pass *)
(* Regression: The typechecker had forgotten that character types could be
* compared.  *)

fun char_cmp c = #"a" <= c

fun f x y = (x = y) orelse x <= y orelse x = #"A"

val _ =
  if char_cmp #"b" andalso f #"A" #"0" then print "pass" else print "fail"

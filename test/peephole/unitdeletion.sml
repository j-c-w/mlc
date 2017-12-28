(* t-compile: --dump-peephole *)

val x = (print "Hello"; 1)

(* We expect no unit's to be created in this snippet.  *)
(* t-scan-not: new .*/Unit\n.*dup.*\n.*invokespecial.*\n.*pop.* : peephole *)

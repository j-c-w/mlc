(* t-compile: --dump-typecheck *)
(* t-run: pass *)

fun g 0 x = 1
  | g 1 2 = 2
  | g y z = 3

val _ = if (g 0 1) = 1 andalso
           (g 1 2) = 2 andalso
           (g 1 0) = 3 then print "pass"
                       else print "fail"

(* t-scan: int ->  \(int -> int\) : typecheck *)

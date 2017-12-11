(* t-compile: --dump-lower-tir *)
(* t-run: pass *)
(* Regression: This was an issue with the order of arguments for
* longer curried functions. *)

fun f (x, y) [a, b] z c = (x + z, c + 1.0)

val (x, y) = f (2, "Hello") [1.0, 8.0] 8 0.0

val _ = if (x = 10) andalso (y <= 1.0001) then print "pass" else print "fail"

(* Regression: The local field orders were all screwed up *)
(* t-scan-not: arg0 Ljava/lang/Integer : lower_tir *)

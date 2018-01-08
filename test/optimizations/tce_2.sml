(* t-compile: --f-tce --dump-tce *)
(* t-scan: Number of functions replaced = 0: tail_elim *)

fun f x =
  f (f x)

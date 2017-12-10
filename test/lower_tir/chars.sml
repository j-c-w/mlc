(* t-compile: *)
(* t-run: pass *)

val ch = #"e"
val bh = #"f"
val kh = #"e"

val msg = if ch = bh then "fail"
    else if ch = kh then "pass" else "fail"
val _ = print msg

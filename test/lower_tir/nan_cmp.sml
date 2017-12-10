(* t-compile: *)
(* t-run: Works *)

val nan1 = 0.0 / 0.0
val nan2 = 0.0 / 0.0

val _ = if not (nan1 <= nan2) then
  if not (nan1 >= nan2) then
    if not (nan2 > nan1) then
      if not (nan2 < nan1) then
        print "Works"
      else print "lt failed"
    else print "gt failed"
  else print "gteq failed"
else print "lteq failed"

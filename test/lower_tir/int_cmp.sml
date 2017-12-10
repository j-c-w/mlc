(* t-compile: *)
(* t-run: pass *)

val res =
  if 4 > 3 then
    if 4 >= 1 then
      if 0 < 10 then
        if ~1 <= 2 then
          if ~4 <= ~4 then
            if 2 >= 2 then
              "pass"
            else "geq"
           else "leq"
        else "leq"
      else "lt"
    else "geq"
 else "gt"

val _ = print res

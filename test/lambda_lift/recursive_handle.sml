(* t-compile: *)
(* Regression: functions with arguments were improperly updating already
* lifted calls to them.  *)

exception e;

fun t _ =
  let fun  g _ = (1) handle e => g 1
  in 1
  end


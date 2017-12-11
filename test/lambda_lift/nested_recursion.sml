(* t-compile: *)
(* t-run: *)
(* Regression: 'g' was originally replaced twice.  There is no scan
* here as this is now asserted internally.  This will fail when running
* if there is another issue.  *)

fun f x =
let 
  val z = 1
  fun g x = if x = 0 then z else z + g (x - 1)
in
  g 1
end

val _ = f ()

(* t-compile: *)
(* Regression: This was not lowered properly as the wrong
* ast environment  was being passed into the let. *)

val x =
  let 
  in 
    1 + 100
  end

(* t-compile: --verify-all *)
(* Regression: The pass inserting the updated calls to 'g' was allowing
* the two udpated calls to alias.  Then, when the variable uniqueifier
* came along for g, it updated the variable for g, and also updated
* the variable for 'h' at the same time.
*
* This resulted in a type environment error.
*)

val x = 
let
  val in1 = true
  fun g () =
    if (in1) then
      []
    else
      g ()

  fun h () =
    if (in1) then
      []
    else
      g ()
in
  1
end


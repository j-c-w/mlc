(* t-compile: *)
(* Regression: The Type environment structure was not entirely hierarchical.
 * The outliner picked this up.  The solution to this is in the lowerAST
 * pass.  We previously lowered if statements into case statements, but
 * this caused consistency problems.  *)

exception f of string
fun h () =
let 
  fun g x =
    if x then
      ()
    else
      ()
      handle f ("find") => ()
in g
end


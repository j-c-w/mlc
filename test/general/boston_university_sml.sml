(* t-compile: *)
(* t-compile: -O *)
(* Lifted from https://www.cs.bu.edu/~hwxi/academic/courses/CS520/Fall02/notes/SMLintro.pdf *)

fun zero_finder (f: int -> int) =
let
  fun aux (i: int): int =
    if f(i) = 0 then i
    else if i > 0 then aux (~i)
    else aux (~i+1)
in
  aux (0)
end

fun foreach (f: 'a -> unit) =
  fn [] => () | x :: xs => (f (x); foreach f xs)

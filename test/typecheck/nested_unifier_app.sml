(* t-compile: *)
(* t-run: *)

(* This was taken from the Knuth-Bendix algorithm benchmark.
 * There was a regression in the typechecker where unifiers
 * were not appropriately applied all the way down a tree.  *)

fun partition p =
  let fun part_rec   []   = ([],[])
        | part_rec (a::L) =
            let val (pos,neg) = part_rec L in
              if true then  ((a::pos), neg) else (pos, (a::neg))
            end
  in part_rec
  end

val _ = partition (fn x => x mod 2 = 0) [1, 2, 3, 4]

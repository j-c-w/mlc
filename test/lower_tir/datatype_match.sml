(* t-compile: *)
(* t-compile: -O *)
(* t-run: pass *)

datatype tree = Leaf of int
              | Node of tree * tree * int

fun sum(Leaf(x)) = x
  | sum(Node(l, r, x)) = x + sum(l) + sum(r)

val sum = sum(Node(Node(Leaf(1), Leaf(2), 3), Leaf(4), 5))

val _ = if (sum = 15) then print "pass" else print "fail"

(* t-compile: --verify-all *)
(* t-compile: -O --verify-all *)
(* t-run: pass *)
(* These are examples lifted from the University of Edinburgh's notes,
* found here http://homepages.inf.ed.ac.uk/stg/NOTES/notes.pdf *)

fun reduce (g, e, m, n, f) =
  case m > n of true => e
  | false => g (reduce(g, e, m, n-1, f), f n)

fun insert (x, [ ]) = [ x ]
  | insert (x, h :: t) = if x <= h
                         then x :: h :: t
                         else h :: insert (x, t)

fun sort [ ] = [ ]
  | sort (h :: t) = insert (h, sort t)

fun prefix [] l = true
  | prefix (a :: l) (b :: m) = a = b andalso prefix l m

(* My own stuff, added for verification of a correct compile.  *)
fun sorted [] = true
  | sorted [x] = true
  | sorted (x :: y :: xs) = x <= y andalso sorted (y :: xs)

val reduced = reduce ((fn (x, y) => x + y), 0, 1, 10, (fn x => x))
val _ = if (prefix [1, 2] [1, 2, 3] andalso reduced = 55
            andalso sorted(sort([4, 3, 2, 1]))) then print "pass"
                                                 else print "fail"

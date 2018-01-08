(* t-compile: --verify-all *)
(* Taken from: https://www.cs.cmu.edu/~rwh/introsml/samplecode/fcnls.sml *)
(* Minor changes required to make this compile.  *)

(* Functionals *)

val x = 2            (* x=2 *)
val y = x*x          (* y=4 *)
val x = y*x          (* x=8 *)
val y = x*y          (* y=32 *)

val x = 2
fun f y = x+y
val x = 3
val z = f 4

fun add (x, y) = x + y
fun times (x, y) = x * y

fun map' (f, nil) = nil
  | map' (f, h::t) = (f h) :: map' (f, t)

val constantly = fn k => (fn a => k)
fun constantly k a = k

fun map f nil = nil
  | map f (h::t) = (f h) :: (map f t)

fun curry f x y = f (x, y)

fun map f l = curry map' f l

fun add_em nil = 0
  | add_em (h::t) = h + add_em t

fun mul_em nil = 1
  | mul_em (h::t) = h * mul_em t

fun reduce (base, opn, nil) = base
  | reduce (base, opn, h::t) = opn (h, reduce (base, opn, t))

fun add_em l = reduce (0, add, l)
fun mul_em l = reduce (1, times, l)

fun consFun (x, xs) = x :: xs

fun mystery l = reduce (nil, consFun, l)

fun better_reduce (base, opn, l) =
    let
        fun red nil = base
          | red (h::t) = opn (h, red t)
    in
        red l
    end

fun staged_reduce (base, opn) =
    let
        fun red nil = base
          | red (h::t) = opn (h, red t)
    in
        red
    end

fun curried_reduce (base, opn) nil = base
  | curried_reduce (base, opn) (h::t) = opn (h, curried_reduce (base, opn) t)

fun append (nil, l) = l
  | append (h::t, l) = h :: append(t,l)

fun curried_append nil l = l
  | curried_append (h::t) l = h :: (curried_append t l)

fun staged_append nil = (fn l => l)
  | staged_append (h::t) =
    let
        val tail_appender = staged_append t
    in
        fn l => h :: tail_appender l
    end

(* t-compile: --verify-all *)
(* This was taken from :
* https://www.cs.cmu.edu/~rwh/introsml/samplecode/fcns.sml *)

(* Some changes in bracketing required to make it compile.  *)

(* Functions *)

fun sqrt x = Math.sqrt(x)

val _ = fn x : real => sqrt (sqrt x)

val _ = (fn x : real => sqrt (sqrt x)) (4.0)

val fourthroot = (fn x : real => sqrt (sqrt x))

fun double (n:int):int = n + n
fun square (n:int):int = n * n
fun halve (n:int):int = n div 2
fun is_even (n:int):bool = (n mod 2 = 0)

fun f(x:real):real = x+x
fun g(y:real):real = y+y

val x = 2.0
fun h(x:real):real = x+x
fun i(y:real):real = x+y

fun j(z:real):real = x+z
fun k(x:real):real = x+x

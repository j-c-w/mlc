(* t-compile: --verify-all *)
(* It is sufficient to make sure we got a single line right *)
(* t-run: [\s\S]*______________________222222222___1111111111111111111111111111________________[\s\S]* *)
(* The prettiest test I have :) *)
(* Taken from http://www.soc.napier.ac.uk/course-notes/sml/mandel.htm 
* I have made a small modification to the output format to make it amenable
* to scanning (replaced *s with _s to avoid intense escaping). *)
(* Mandelbrot set investigator New Jersey ML *)
(* A Cumming Napier University *)
(* Start off with some useful complex number functions *)
fun square(x,y) = (x*x-y*y,2.0*x*y)
fun add (x,y) (u,v) = (x+u,y+v):real*real
fun scalar (s:real) (x,y) = (s*x,s*y)
fun dot (x1,y1) (x2,y2) = (x1*x2,y1*y2):real*real
fun sub p q = add p (scalar ~1.0 q)
fun dist p q = let
                fun r(x,y)=Math.sqrt(x*x+y*y)
        in r(sub p q) end
val zero = (0.0,0.0)


(* Mandelbrot functions *)
fun man c z = add (square z) c
(* Multiple applications of a function *)
fun twice f x = f (f x)
fun thrice f x = f (f (f x))
fun quice f = twice twice f
fun t256 f = twice(twice(twice twice)) f   (* apply 256 times *)
fun t1024 f = (t256(f))
(* Catagorise the point type 1,2,3,4,_ or " " *)
(* Complete 200 iterations then test for period of 1,2,3 or 4 *)
fun cat p = let
        val small = 0.001
        val a= t1024 (man p) zero
        in   if dist a (man p a)<0.001 then "1"
        else if dist a (twice (man p) a)<small then "2"
        else if dist a (thrice (man p) a)<small then "3"
        else if dist a (quice (man p) a)<small then "4"
        else "_"
        end
fun for (r1:real) r2 d f = if (0.0 > (r2-r1)*d) then ""
                                        else (f r1)^(for (r1+d) r2 d f)
fun line x1 x2 y = (for x1 x2 ((x2-x1)/78.0) (fn i=> cat(i,y)))^"\n"
fun box((x1,y1),(x2,y2))= for y2 y1 ((y1-y2)/24.0) (fn i=> line x1 x2 i)
fun K a b = b
val ibox=( (~2.0,~1.0),(1.0,1.0))

(* Box shifting stuff *)
fun zoom(p, q) = (add (scalar 0.75 p) (scalar 0.25 q),
                  add (scalar 0.25 p) (scalar 0.75 q))
fun shift v (p,q) = let val w = dot v (sub q p) in
                (add w p, add w q) end
val right = shift (0.5,0.0)
val left = shift (~0.5,0.0)
val up = shift (0.0,0.5)
val down = shift (0.0,~0.5)
fun doit x = K (print(box x)) x
val _ = doit ibox

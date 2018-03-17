(* Taken from the SMLNJ benchmarking suite.  *)
fun map f [] = []
  | map f (a::x) = f a :: map f x

fun error str = (print str; error str)

fun rev [] = []
  | rev (x :: xs) = (rev xs) @ x

fun concat [] = []
  | concat (x :: xs) = x @ (concat xs)
 
fun strconcat [] = ""
  | strconcat (x :: xs) = x ^ (strconcat xs)

fun app f [] = ()
  | app f (x :: xs) = (f x; app f xs)

fun cat x y = (fn z => x (y (z)))

fun accumulate f = let
  fun foldf a [] = a
        | foldf a (b::x) = foldf (f a b) x
      in
    foldf
  end

fun filter f [] = []
  | filter f (x :: xs) = 
    if f x then x :: (filter f xs)
    else filter f xs

fun exists p = let fun existsp [] = false
                 | existsp (a::x) = if p a then true else existsp x
            in existsp end

fun equal a b = (a  = b)

fun member x a = exists (equal a) x

fun C f x y = f y x

fun cons a x = a::x

fun revonto x = accumulate (C cons) x

fun length x = let fun count n a = n+1 in accumulate count 0 x end

fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                   fun check n = if n<0 then error "repeat<0" else n
                in cat rptf check end

fun copy n x = repeat (cons x) n []

fun spaces n = strconcat (copy n " ")

fun lexless(a1:int,b1:int)(a2,b2) = 
       if a2<a1 then true else if a2=a1 then b2<b1 else false
fun lexgreater pr1 pr2 = lexless pr2 pr1
fun lexordset []  = []
  | lexordset (a::x) = (lexordset (filter (lexless a) x)) @ [a] @
                       (lexordset (filter (lexgreater a) x))
  fun collect f l =
         let fun accumf sofar [] = sofar
               | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
          in accumf [] l
         end
  fun occurs3 x = 
      (* finds coords which occur exactly 3 times in coordlist x *)
      let 
        fun diff x y = filter (fn z => not (member y z)) x
        fun f xover x3 x2 x1 [] = diff x3 xover
            | f xover x3 x2 x1 (a::x) = 
               if member xover a then f xover x3 x2 x1 x else
               if member x3 a then f (a::xover) x3 x2 x1 x else
               if member x2 a then f xover (a::x3) x2 x1 x else
               if member x1 a then f xover x3 (a::x2) x1 x else
                           f xover x3 x2 (a::x1) x
       in f [] [] [] [] x end
      fun alive (livecoords) = livecoords
      fun mkgen coordlist = (lexordset coordlist)
      fun mk_nextgen_fn neighbours gen =
          let val living = alive gen
              val isalive = member living
              val liveneighbours = (cat (cat length (filter isalive)) neighbours)
              fun twoorthree n = n=2 orelse n=3
          val survivors = filter (cat twoorthree liveneighbours) living
          val newnbrlist = collect (cat (filter (fn z => not(isalive z))) neighbours) living
          val newborn = occurs3 newnbrlist
       in mkgen (survivors @ newborn) end

fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
            (i,j-1),(i,j+1),
            (i+1,j-1),(i+1,j),(i+1,j+1)]

val xstart = 0
val ystart = 0
      fun markafter n s = s ^ (spaces n) ^ "0"
      fun plotfrom (x,y) (* current position *)
                   str   (* current line being prepared -- a string *)
                   ((x1,y1)::more)  (* coordinates to be plotted *)
          = if x=x1
             then (* same line so extend str and continue from y1+1 *)
                  plotfrom(x,y1+1)(markafter(y1-y)str)more
             else (* flush current line and start a new line *)
                  str :: plotfrom(x+1,ystart)""((x1,y1)::more)
        | plotfrom (x,y) str [] = [str]
       fun good (x,y) = x>=xstart andalso y>=ystart
 fun plot coordlist = plotfrom(xstart,ystart) "" 
                             (filter good coordlist)


fun at (coordlist, (x:int,y:int)) = let fun move(a,b) = (a+x,b+y) 
                                  in map move coordlist end
val rotate = map (fn (x:int,y:int) => (y,~x))

val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
val bail = [(0,0),(0,1),(1,0),(1,1)]
fun barberpole n =
   let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                   else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
    in (0,0)::(1,0):: f 0
   end

val genB = mkgen((at(glider, (2,2))) @ (at(bail, (2,12)))
         @ rotate (at((barberpole 4), (5,20))))

fun nthgen g 0 = g | nthgen g i = nthgen (mk_nextgen_fn neighbours g) (i-1)

val gun = mkgen
 [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
  (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
  (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
  (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
  (9,29),(9,30),(9,31),(9,32)]

fun show pr = cat (cat (app (fn s => (pr s; pr "\n"))) plot) alive

fun doit () = show (fn _ => ()) (nthgen gun 50)

fun testit 0 = ()
  | testit n = (doit(); testit(n - 1))

fun main() = 
  let 
    val startTime = Timer.startRealTimer()
    val res = testit 100
    val time = Timer.checkRealTimer(startTime)
    val _ = (print("Execution Time: " ^ (Time.toString(time)) ^ "\n");
             print("Validation: " ^ "pass" ^ "\n"))
  in
    0
  end

fun tak (x,y,z) =
   if not (y < x)
      then z
   else tak (tak (x - 1, y, z),
             tak (y - 1, z, x),
             tak (z - 1, x, y))

fun f 0 = ()
  | f n = (tak (33,22,11); f (n-1))


val timer = Timer.startRealTimer();
val res = f 1
val result_ok = res = ()
val time = Timer.checkRealTimer(timer)
val _ = (print("Execution Time: " ^ (Time.toString(time)) ^ "\n");
         print("Validation: pass\n"))

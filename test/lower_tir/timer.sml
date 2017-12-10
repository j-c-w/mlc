(* t-compile: *)
(* Expect some non-zero output.  *)
(* t-run: ([1-9][0-9]*\.[0-9]*|0\.[0-9]*[1-9][0-9]* ?) *)

fun count 0 = true
  | count n = count (n - 1) andalso count (n - 1)

val timer = Timer.startRealTimer()
val _ = count 17
val time = Timer.checkRealTimer(timer)
val _ = print (Time.toString(time))

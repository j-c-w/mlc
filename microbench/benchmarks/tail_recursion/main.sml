(* This is a benchmark designed to have vast improments upon ellimination
* of tail recursion. *)

fun id x = x

fun tail_fib (0, sum, tail) = tail(sum + 1)
  | tail_fib (1, sum, tail) = tail(sum + 1)
  | tail_fib (n, sum, tail) =
        tail_fib(n - 1, sum, fn(sum) => tail_fib(n - 2, sum, tail))


fun do_sum () = ((tail_fib (36, 0, id)) = 24157817)

fun main () =
  let 
    val timer = Timer.startRealTimer()
  in
    let
      (* Use 1 as the seed for the RNG. This makes the results
         repeatable, but no constant foldable.  *)
      (* This yeilds the reverse of the traditional Fourier Transform. *)
      val result = do_sum()
  in 
      let
        val time = Timer.checkRealTimer(timer)
        val result_ok = (if (result) then "pass" else "fail")
      in
        (print("Execution Time: " ^ Time.toString(time) ^ "\n");
        print("Validation: " ^ result_ok ^ "\n"))
      end
    end
  end

val a = main ()

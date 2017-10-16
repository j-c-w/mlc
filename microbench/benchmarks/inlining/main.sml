(* This is a benchmark designed to have vast improments upon inlining *)

fun id x = x
fun add x y = (id x) + (id y)
fun add_add x y = (add (add x y) (add x y))
fun add3 x y = (add_add (add_add x y) (add_add x y))
fun add4 x y = (add3 (add3 x y) (add3 x y))
fun add5 x y = (add4 (add4 x y) (add4 x y))
fun add6 x y = (add5 (add5 x y) (add5 x y))
fun add7 x y = (add6 (add6 x y) (add6 x y))
fun add8 x y = (add7 (add7 x y) (add7 x y))
fun add9 x y = (add8 (add8 x y) (add8 x y))
fun add10 x y = (add9 (add9 x y) (add9 x y))
fun add11 x y = (add10 (add10 x y) (add10 x y))
fun add12 x y = (add11 (add11 x y) (add11 x y))
fun add13 x y = (add12 (add12 x y) (add12 x y))
fun add14 x y = (add13 (add13 x y) (add13 x y))
fun add15 x y = (add14 (add14 x y) (add14 x y))
fun add16 x y = (add15 (add15 x y) (add15 x y))
fun add17 x y = (add16 (add16 x y) (add16 x y))

fun do_add () = ((add16 0 0) = 0)


fun main () =
  let 
    val timer = Timer.startRealTimer()
  in
    let
      (* Use 1 as the seed for the RNG. This makes the results
         repeatable, but no constant foldable.  *)
      val length = 1000
      (* This yeilds the reverse of the traditional Fourier Transform. *)
      val result = do_add()
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

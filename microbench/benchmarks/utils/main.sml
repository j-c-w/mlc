fun member [] _ = false
  | member (x :: xs) n = (x = n) orelse member xs n

fun generate_and_check () = 
let
  val l = [7, 5, 4,3, 6, 7, 8, 3, 2, 6]
in
  member l 2
end

fun sorted [] = true
  | sorted [x] = true
  | sorted  (x :: y :: xs) = x <= y andalso sorted(y :: xs)

fun lt_cmp x y = x < y

fun gteq_cmp x y = x >= y

fun filter f [] = []
  | filter f (x :: xs) =
        if f(x) then x :: (filter f xs) else filter f xs

fun sort [] = []
  | sort [x] = [x]
  | sort (x :: xs) =
    let
      val lt = filter (gteq_cmp x) xs
      val gteq = filter (lt_cmp x) xs
    in
      sort(lt) @ [x] @ sort(gteq)
    end
 
fun sum ([], n) = n
  | sum ((x :: xs), n) = sum(xs, n + x)

fun run_sum 0 = true
  | run_sum n = ((sum ([1, 2, 3, 4, 5, 6, 7, 8, 9], 0)) = 45) andalso (run_sum (n - 1))

fun poly_of x = 
let
  val y = 10
  val z = y + 100
  val k = z + 1000
  val a = z + 1000
  val b = z + 1000 + a
in
  x * y + x * z + x * k + x * a + x * b
end

fun map [] _ = []
  | map (x :: xs) f = f(x) :: map xs f

fun length [] = 0
  | length (x :: xs) = 1 + length(xs)

fun eval_poly () = 
  length(map [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] poly_of) > 4

fun do_test 0 = true
  | do_test n = ((generate_and_check()) andalso
                 sorted(sort([10, 9, 8, 7, 6, 5, 4, 3, 2, 1])) andalso
                 (run_sum 10) andalso
                 (eval_poly ()) andalso
                 do_test (n - 1))

(* Inserted so that the recursion depth does not get too deep for the JVM.  *)
fun do_test_wrapper 0 = true
  | do_test_wrapper n =
        do_test 1000 andalso do_test_wrapper (n - 1)

fun main () = 
  let 
    val timer = Timer.startRealTimer()
  in
    let
      val result = do_test_wrapper 100
      val res_str = if (result) then "pass" else "fail"
    in 
      let
        val time = Timer.checkRealTimer(timer)
      in
        (print("Execution Time: " ^ Time.toString(time) ^ "\n");
        print("Validation: " ^ res_str ^ "\n"))
      end
    end
  end

val a = main ()

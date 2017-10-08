fun zip [x] [y] = [(x, y)]
  | zip (x :: xs) (y :: ys) = (x, y) :: (zip xs ys)

fun fft_list (n, random) = 
    let 
      val x = Random.randomlist(n, random)
      val y = Random.randomlist(n, random)
    in
      zip x y
    end

fun element_div((x, y), c) = (x / c, y / c)

fun pow2(x:real) = x * x

fun complex_times ((x1: real, y1: real), (x2: real, y2: real)) =
  (x1 * x2 - y1 * y2, x1 * y2 + y1 * x2)

fun complex_sum((x1, y1), (x2, y2)):real * real = (x1 + x2, y1 + y2)

fun complex_div((x, y), (a, b)): real * real =
  element_div(complex_times((x, y), (a, ~b)), pow2(x) + pow2(y))

fun e_pow((x1, y1)) = (Math.cos(x1), Math.sin(y1))

fun modulus(x, y) = Math.sqrt(pow2(x) + pow2(y))

fun at ((x :: xs), 0) = x
  | at ((_ :: xs), n) = at(xs, (n - 1))

fun fourierElement _ _ ~1 _ = (0.0, 0.0)
  | fourierElement (input: (real * real) list) totalLength k n = 
let
  val pi = 3.1415926
  val exponent = complex_times((0.0, 1.0),
                               (2.0 * pi * Real.fromInt(n) * Real.fromInt(k) /
                               Real.fromInt(totalLength), 0.0))
in 
  complex_sum(complex_times(at(input, k), e_pow(exponent)),
              (fourierElement input totalLength (k - 1) n))
end

fun fft x fourierLength 0 = []
  | fft x fourierLength n =
      (element_div((fourierElement x fourierLength (fourierLength - 1) n),
                   Real.fromInt(fourierLength)))
      :: (fft x fourierLength (n - 1))

fun fft_wrapper (x: (real * real) list) (n: int) = fft x n n

fun check_result ([], sum) = (sum > 650.0) andalso (sum < 750.0)
  | check_result ((x :: xs), sum) =
        check_result (xs, (sum + modulus(x)))

fun main () =
  let 
    val timer = Timer.startRealTimer();
  in
    let
      (* Use 1 as the seed for the RNG. This makes the results
         repeatable, but no constant foldable.  *)
      val length = 1000
      (* This yeilds the reverse of the traditional Fourier Transform. *)
      val result = fft_wrapper
                    (fft_list(length, (Random.newgenseed(1.0)))) length
    in 
      let
        val time = Timer.checkRealTimer(timer)
        val result_ok = (if (check_result(result, 0.0)) then "pass" else "fail")
      in
        (print("Execution Time: " ^ Time.toString(time) ^ "\n");
        print("Validation: " ^ result_ok ^ "\n"))
      end
    end
  end

val _ = main ();

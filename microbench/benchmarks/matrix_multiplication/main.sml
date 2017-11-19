fun generateList seed 0 = []
  | generateList seed n =
        seed :: (generateList(seed + 1.0) (n - 1))

fun generateMatrix seed 0 _ = []
  | generateMatrix seed n max =
        (generateList seed max) :: (generateMatrix (seed + 1.0) (n - 1) max)

fun dot_prod ([x], [y]) = x * y: real
  | dot_prod (x :: xs, y :: ys) = x * y

fun transpose_loc ([]) base = base
  | transpose_loc (x :: xs) base =
    let
      fun prependList [] [] = []
        | prependList (x :: xs) (y :: ys) = (x :: y) :: (prependList xs ys)

    in
      prependList x (transpose_loc xs base)
    end

fun transpose ls =
let
  fun newBase [] = []
    | newBase (_ :: xs) = [] :: newBase(xs)
in
  transpose_loc ls (newBase(ls))
end

fun mat_mul m1 m2 =
let
  fun do_mult (row, []) = []
    | do_mult (row, (x :: transposed)) =
          (dot_prod(row, x)) :: (do_mult(row, transposed))

  fun mult_rows [] transposed = []
    | mult_rows (x :: xs) transposed =
        (do_mult(x, transposed)) :: (mult_rows xs transposed)
in
  mult_rows m1 (transpose(m2))
end

fun sum [] = 0.0
  | sum (x :: xs) = x + (sum xs)

fun mat_sum [] = 0.0
  | mat_sum (x :: xs) = sum(x) + (mat_sum xs)

fun main () =
  let 
    val timer = Timer.startRealTimer()
  in
    let
      val m1 = generateMatrix 1.1 1000 1000
      val m2 = generateMatrix 300000.1 1000 1000
      val result = mat_sum((mat_mul m1 m2))
  in 
      let
        val time = Timer.checkRealTimer(timer)
        val result_ok = if (result > 1.504300E14 andalso result < 1.504304E14)
                        then "pass" else "fail"
        val _ = print(Real.toString(result))
      in
        (print("Execution Time: " ^ Time.toString(time) ^ "\n");
        print("Validation: " ^ result_ok ^ "\n"))
      end
    end
  end

val a = main ()

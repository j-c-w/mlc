val list2 =
  ["A", "C", "G", "T", "G", "T", "T", "C", "G", "G", "G", "A", "G", "G", "G",
  "A", "A", "G", "A", "G", "G", "A", "G", "A", "G", "A", "G", "G", "A", "G",
  "G", "A", "G", "C", "G", "C", "G", "A", "G", "A", "G", "G", "C", "G", "G",
  "C", "G", "A", "G", "G", "A", "G", "C", "G", "A", "T", "T", "C", "T", "T",
  "A", "T", "T", "A", "T", "A", "G", "C", "A", "G", "G", "A", "C", "T", "T",
  "A", "G", "C", "A", "T", "A", "G", "T", "C", "A", "G", "A", "T", "C", "G",
  "A", "T", "G", "C", "A", "C", "G", "T", "G", "T", "C", "A", "T", "G", "C",
  "A", "T", "G", "C", "A", "T", "A", "A", "T", "C", "T", "A", "T", "C", "C",
  "G", "T", "A", "T", "A", "G", "T", "C", "A", "G", "T", "G", "G", "T", "A",
  "T", "C", "G", "C", "G", "A", "C", "A", "T", "C", "G", "C", "T", "A", "T",
  "G", "A", "C", "G", "T", "A", "C", "T", "A", "C", "T", "G", "T", "G", "T",
  "G", "T", "C", "G", "T", "A", "T", "G", "A", "C", "G", "T", "A", "C", "G",
  "T", "A", "C", "G", "T", "A", "T", "G", "A", "C", "G", "T"]
val list1 =
  ["A", "C", "G", "A", "T", "C", "G", "T", "A", "C", "G", "G", "T", "A", "C",
  "A", "T", "C", "G", "A", "T", "G", "T", "A", "C", "G", "T", "A", "C", "T",
  "G", "A", "C", "G", "T", "C", "A", "T", "A", "T", "C", "G", "T", "A", "C",
  "T", "A", "T", "G", "C", "T", "A", "G", "T", "C", "A", "T", "T", "A", "T",
  "T", "C", "G", "C", "A", "T", "G", "A", "C", "T", "T", "T", "C", "A", "G",
  "G", "C", "G", "T", "G", "A", "T", "T", "A", "G", "C", "T", "G", "T", "T",
  "A", "T", "C", "G", "G", "A", "A", "C", "G", "C", "G", "G", "A", "G", "G",
  "A", "T", "C", "G", "T", "G", "G", "T", "G", "A", "T", "C", "G", "T", "A",
  "G", "T", "G", "T", "A", "G", "T", "G", "T", "C", "A", "G", "T", "C", "G",
  "T", "A", "C", "G", "T", "G", "A", "T", "G", "T", "G", "T", "A", "C", "G",
  "T", "A", "G", "T", "G", "A", "C", "G", "T", "A", "G", "T", "A", "C", "G",
  "T", "G", "T", "A", "G", "A", "G", "C", "A", "G", "T", "C", "G", "T", "A",
  "G", "T", "A", "G", "T", "G", "T", "A", "C", "G", "T", "A", "C", "G", "T",
  "G", "T", "A", "G", "T", "G", "T", "A", "G", "T", "C", "A", "G", "T", "G",
  "T", "A", "G", "T", "A", "G", "T", "C", "G", "T", "C", "A"]

fun scoring "Insertion" "" = ~4
  | scoring "Deletion" "" = ~4
  | scoring x y =
    if (x = y) then 5
    else ~3

fun lengthN([], n) = n
  | lengthN(x :: xs, n) = lengthN(xs, 1 + n)

fun length xs = lengthN(xs, 0)

fun max(x, y, z) =
  if x > y then if z > x then z else x else if z > y then z else y

fun at (x :: xs) 0 = x
  | at (_ :: xs) n = at xs (n - 1)

fun build (scoring, in1, in2) =
let
  fun add_cell so_far (0, 0) = 0
    | add_cell so_far (0, n) = n * (scoring "Insertion" "")
    | add_cell so_far (n, 0) = n * (scoring "Deletion" "")
    | add_cell so_far (i, j) =
        max((so_far(i - 1, j - 1)) + (scoring (at in1 (i - 1)) (at in2 (j - 1))),
            (so_far(i - 1, j)) + (scoring "Deletion" ""),
            (so_far(i, j - 1)) + (scoring "Insertion" ""))

  fun build_matrix_row current_matrix current_row =
    if length(current_row) = length(in1) + 1 then
      current_row
    else
      let
        fun so_far (i, j) = 
          if length(current_matrix) <= j then
            at current_row i
          else
            at (at current_matrix j) i
      in
        build_matrix_row current_matrix
          (current_row @
            [(add_cell so_far
              (length(current_row), length(current_matrix)))])
      end

  fun build_matrix current_matrix =
    if length(current_matrix) = length(in2) + 1 then
      current_matrix
    else
      build_matrix (current_matrix @ [(build_matrix_row current_matrix [])])
in
  build_matrix []
end


fun main () =
  let 
    val timer = Timer.startRealTimer()
  in
    let
      val result = build(scoring, list1, list2)
  in 
      let
        val time = Timer.checkRealTimer(timer)
        val result_num =
          at (at result ((length(list2)) - 1)) (((length(list1)) - 1))
        val result_ok = if (result_num = 307) then "pass" else "fail"
      in
        (print("Execution Time: " ^ Time.toString(time) ^ "\n");
        print("Validation: " ^ result_ok ^ "\n"))
      end
    end
  end

val a = main ()

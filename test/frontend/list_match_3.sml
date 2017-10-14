(* t-compile: *)

fun list_cont [x] [y] = [(x, y)]
  | list_cont [x, y] [a, b] = [(x, y), (a, b)]


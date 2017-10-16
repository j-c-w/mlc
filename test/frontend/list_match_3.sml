(* t-compile: *)

fun mylist_cont [x] [y] = [(x, y)]
  | mylist_cont [x, y] [a, b] = [(x, y), (a, b)]


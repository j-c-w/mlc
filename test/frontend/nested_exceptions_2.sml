(* t-compile: *)

val aExp =
  let exception a of int;
  in
    a
  end

val _ = raise aExp(1)

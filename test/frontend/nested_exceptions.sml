(* t-compile: *)
(* t-fail *)

val _ =
  let
    exception a
  in
    1
  end

val _ = raise a

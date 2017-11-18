(* t-compile: --dump-lower-program *)

val x = let
  val y =
    let val z =
        let val a =
            let val b = 
                let val c =
                    1
                 in c end
            in b end
        in a end
    in z end
  in y end

(* t-scan-not: let: lower_program *)

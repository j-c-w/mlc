(* t-compile: --dump-lower-program *)

fun f x =  
  let val y = 1
  in 
    let val z = 2
    in 
      y + z
    end
  end

(* t-scan-not: let : lower_program *)
(* t-scan-times-5: Assign: lower_program *)

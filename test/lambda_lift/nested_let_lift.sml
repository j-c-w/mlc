(* t-compile: --run-lambda-lift-verify --dump-lambda-lift *)

val x =
  let 
    val t = let
      val a = 1
      val b = let
        val c = 10
        fun f () = c + a
      in
        f
      end
    in
      b
    end
    fun y x = (t()) + x
  in
    (1; y)
  end

(* We check that the closure has been constructed by checking that
* 'end' does not immediately follow the 'f' on line 11. *)
(* t-scan-not: f\s*end: lambda_lift *)

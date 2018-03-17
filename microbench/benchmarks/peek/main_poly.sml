(* Written by Stephen Weeks (sweeks@sweeks.com). *)
(* Modified to be immutable by Jackson Woodruff.  *)
(* Timing utilities and other minor adaptations to run with CMLC by
 * Jackson Woodruff.  *)
      datatype t = T of exn list
      datatype intopt = SOME of int
                   | NONE

      exception ValOfNone

      fun valOf (SOME(x)) = x
        | valOf NONE = raise ValOfNone

      fun new () = T ([])

      fun addPeek () =
         let
            exception E of int
            fun add (T r, x) = T(E x :: r)
            fun peek (T r) =
               let
                 fun loop [] = (NONE, T ([]))
                      | loop((E x) :: l) = (SOME x, T l)
                      | loop(_ :: l) = loop l
               in loop (r)
               end
         in ( add, peek )
         end

      fun inner () =
         let
            val l = new ()
            val ( add, peek ) = addPeek ()
            val res = add (l, 13)
            fun loop (i, ac, l) =
               if i = 0
                  then ac
               else
                 let
                   val (hd, l')  = peek l
                 in
                   loop (i - 1, ac + valOf (hd), l)
                 end
            val n = loop (1000000, 0, res)
         in n = 13000000
         end

      fun doit size =
         let
            fun loop i =
               if i = 0
                  then true
               else (inner () andalso loop (i - 1))
         in loop size
         end
fun main() =
  let
     val startTime = Timer.startRealTimer()
     val pass = doit(100)
     val endTime = Timer.checkRealTimer(startTime)

     val passStr = if pass then "pass" else "fail"

     val _ = print("Execution Time: " ^ Time.toString(endTime) ^ "\n" ^
                   "Validation: " ^ passStr)
  in
    0
  end

(* t-compile: *)
(* This was taken from PolyML's testsuite. Test087 in the suceed cases.  *)

val x = let
    fun f x = x
    val c = (f, f)
    val (a, b) = c
in
    a(b 1)
end

val x = let
    val c = (nil, nil)
    
    fun f(a::_, b::_) = a+b | f _ = 0
in
    f c
end

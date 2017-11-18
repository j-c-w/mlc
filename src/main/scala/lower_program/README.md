This pass takes a structure of the program an converts
into only function delcarations.  It must be run
after the lambda_lift pass, and is a prerequisite
to the let flattening pass.

It takes a program that is structured like this:

	val x = 10
	fun f x = x

	val y = f x

And restructures it into:

	fun main () =
		let
			val x = 10
			...
		in
			()
		end

	fun f x = x

This pass implements lambda lifting. This is a compilation technique
that involves lifing nested functions out of their delcaration
sites.

So, something like this:

	
	fun f x = let
		val y = 10
		fun g () = y
	in
		g()
	end

Becomes:

	fun g y () = y

	fun f x = let
		val y = 10
	in g y
	end


After this pass has executed, there will be no function declarations
left within val declarations.

Names of functions will have already been uniqueified at this point,
so this pass does not need to worry about renaming.

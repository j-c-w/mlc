This pass lowers from the AST representation to the TIR representation.

The things that are changed in this pass are:

	- Constants are converted into machine representable versions
	(so reals are put into doubles and ints into ints)

	- We convert the 'ASTExpInfixApp' and 'ASTUnOpApply' construction into
	a normal function application construction.

Things that immediately follow this pass are:

	- Curried functions are put in a better format for manipulation.
	In the AST format, they are represented as:

		fun f i r s = s

		Fun(int, Fun(real, Fun(string, string)))

	Whereas in TIR this would be represented as:
	
		Fun(List(int, real, string), string)

	- We also do lambda lifting in this reduction stage. So, nested function
	declarations get mangled function names and get lifted to the top level:

	val x = let
		fun f x = ...
	in
		f x
	end

	->

	fun $f x = ...

	val x = $f x

	- We rename functions and vals so that each function/val name is unique.
	This allows us to un-interleave the definitions of functions/vals.


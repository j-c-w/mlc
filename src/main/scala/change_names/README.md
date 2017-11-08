This pass is designed to change function and val names so that
they are unique.

It also keeps them readable by postfixing numbers onto names.
So, for example:

	fun f x = let
		fun f x = x
	in
		f x
	end

Becomes:

	fun f1 x2 = let
		fun f3 x4 = x4
	in
		f3 x2
	end

This pass works by:

1. Walk the tree in a top to bottom fashion. At each function or
val declaration, note the name of that fun or val in a map and
replace it with a new (unique) name.

2. At each identifier, check if the identifier is the in substitution
map. It it is, then make a substitution. 

Note that in a full dialect of ML, we would have to be careful to make
the right renaming for forward uses of variables. An easy case of this is

	fun f x = g x
	and g x = 10

Where we have to ensure that both definitions of 'g' get the right value.

A harder  case is this:

	fun g x = x + 1

	fun f x = g "HI"
	and g x = x

Where the call the `g "HI"` actually refers to the second delcaration of
`g x`.

This is not a case currently handled by this class, since `and` is not
a keyword in this subset.

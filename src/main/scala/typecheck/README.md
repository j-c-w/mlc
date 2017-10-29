This is a typecheking pass implementing the static semantics of
SML.

We implement Hindley-Milner, infering types as needed.

This pass also rejects language features that are parsed but
not supported (datatypes).

Typechecking decisions:

	- We approach explicit typing in the Ocaml approach. That is, we
	accept:

		val x: 'a = 4

	As a valid type (as it is unified rather than specialized to)

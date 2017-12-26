This is a peephole pass.  It is run after the code generation pass.

It is slightly more complicated than a traditional peephole pass.
That is, if we have something like this:

	if_cmpeq L1:
	... (Code) ...
	Instruction_1
	goto L1
	... (Code) ...
	Instruction_1
	L1:
	Instruction_2

Then the pair of (Instruction_1, Instruction_2) is considered as a peephole.
This is only done if it is safe to do so.

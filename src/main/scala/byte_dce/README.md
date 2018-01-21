The lower tir pass inserts many redundant writes to local variables.

This pass is primarily to eliminate those writes.  However, it can also
eliminate other dead stores.  Analysis is currently more defensive
than it needs to be.  For example, there is no reason that this pass
cannot handle:

	iload_1
	BOX
	Store

This is currently ignored due to the function call, but we could make
special cases for box and unbox calls.


This pass should be accompanied by a DCE pass in the TIR.  It is very
hard to determine whether a store is safe to remove if there is
flow control (i.e. a label mark) in the part for loading the new value.
These more complicated cases should really be handled in the TIR.

This pass works by building a CFG of the byteR.  The CFG is then analyzed
using the standard LVA algorithm.  This algorithm is used to mark stores
with a StoreStatus (either Live or Dead).  A subsequent pass walks these
stores.  It attempts to find the code used to load arguments for that
instruction.

If the code used to load the arguments is not convoluted (e.g. does not
contain any control flow) the arguments are analyzed for side effects.
If side effects are not found, the store is deleted.  If side effects
are found, the store is turned into a pop.

This is a copy propagation pass.  It eliminates variable copies.
The lower_program pass generates (among other things) a lot
of redundnat copies.

This pass does a copy propagation for variables.  It deletes unused
variables from FunLets.

Top level variables are not currently propagated, although it would be
an easy extension and profitable to do so.

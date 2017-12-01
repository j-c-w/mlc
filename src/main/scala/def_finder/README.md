This is a utility class that goes through and finds the definition
of a variable.  In the current implementatation it is really slow.

Given some variable x, and some program:

val z = 1

fun x _ = 10

val y = x 1

This will return the 'fun x _ = 10'

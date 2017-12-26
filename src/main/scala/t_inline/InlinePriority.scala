package t_inline

/* This is a set classes that list the various priorities that a function
 * call can have for inlining.
 * 
 * Those are:
 *    SINGLE_USE: The function only has a single use (and not exported)
 *
 *    MAYBE_HOT_USE: This is a function call occuring in a possibly hot
 *                   location.
 *
 *    MAYBE_COLD_USE: This is a function call occuring in a probably cold
 *                    location.
 */

sealed trait InlinePriority

case object SINGLE_USE extends InlinePriority
case object MAYBE_HOT_USE extends InlinePriority
case object MAYBE_COLD_USE extends InlinePriority

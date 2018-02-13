package target

import target.jvm._
import t_inline.{InlinePriority,MAYBE_HOT_USE,SINGLE_USE,MAYBE_COLD_USE}

object JVMConfig extends TargetConfig {
  def maxInlineInstructions(priority: InlinePriority) = priority match {
    // Note that this must not go about 10,000.  10,000 is treated as a
    // 'never inline value'
    case SINGLE_USE => 1000
    // On the JVM, the cost of executing a function call can be quite high.
    // It involves:
    //  new
    //  dup
    //  (then probably, for more than one argument)
    //  new Tuple
    //  dup
    //  init
    //  ... Generate and box each arg ... which may or may not be needed anyway
    //  init
    //  invokevirtual
    // So at least 7 instructions.
    // For a maybe cold use, we take this at face value.
    //
    // For a maybe hot use, we note that if the function had a lot of curried
    // arguments, then it is going to be expensive execute by repeated
    // currying.
    //
    // Arbitrarily, we return 4 times this.
    case MAYBE_HOT_USE => 28
    // Currently the cold funciton analysis is very bad.  It does not consider
    // transitively hot functions.  Once that is resolved, this should be
    // lowered.
    case MAYBE_COLD_USE => 28
  }

  def peepholeSet =
    JVMPeephole.getSet
}

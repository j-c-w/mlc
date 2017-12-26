package target

import byteR.JVMInstruction
import peephole.PeepholeSet
import t_inline.InlinePriority

abstract class TargetConfig {
  /* This is not treated as an exact number.  The number of instructions
   * is hard to deduce from the tree at such an early point in time
   * anyway, so it is going to be fudged a little bit.  This can
   * make up for this fudging.
   */
  def maxInlineInstructions(priority: InlinePriority): Int

  def peepholeSet: PeepholeSet[JVMInstruction]
}

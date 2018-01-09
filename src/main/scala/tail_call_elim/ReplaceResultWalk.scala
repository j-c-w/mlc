package tail_call_elim

import tir._
import function_analysis.ExpressionResultWalk

class ReplaceResultWalk(loopID: Int) extends ExpressionResultWalk {
  override def returnExp(exp: TExp) = {
    // This is result value for the input function.  Pass it back up.
    Some(TExpBreak(exp, loopID))
  }
}

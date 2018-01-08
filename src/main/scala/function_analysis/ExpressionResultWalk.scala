package function_analysis

import exceptions.ICE
import recursion_analysis.TParentSetTailPass
import tir._

/* Note that this is distinct from the walk in recursion_analysis.  To see
 * this, consider the following expression:
 *
 *  fun f x = x; x + x
 *
 * The recursion analysis will return the parts that are suitable for tail
 * calls.  In this example 'x; x + x' will be true.  This pass is more
 * specific, adding another flag to the one passed so that the smallest
 * possible expression is chosen to hide in a return value.
 *
 * This is not provided as a walk of the tree.  Rather, it is provided as
 * an implementable callback that fires when a suitable value is reached.
 */
abstract class ExpressionResultWalk extends TParentSetTailPass {
  def returnExp(exp: TExp): Option[TExp]

  override def apply(isTail: Boolean, exp: TExp) = exp match {
    case app @ (_: TExpFunApp | _: TExpConst | _: TExpIdent | _: TExpTuple
                | _: TExpList | _: TExpFn | _: TExpAssign | _: TExpListHead
                | _: TExpListTail | _: TExpTupleExtract | _: TExpListExtract
                | _: TExpListLength) =>
      if (isTail)
        returnExp(app)
      else
        None
    case (_: TExpReturn | _: TExpContinue | _: TExpThrow) =>
      // Hmm. Another hard case that doesn't matter for  the current
      // implementation.  Say these are false is correct I think.  There is
      // no recursion that has to be done here.
      None
    case whileLoop: TExpWhile =>
      // The difficulty with while loops is that they don't really represent
      // a return value... I believe that in a sound version of this walk 
      // for while loops (not currently needed), the while loops should be
      // just walked normally.  This is what is implemented here.
      None
    case exp @ (_: TExpCase | _: TExpFunLet | _: TExpIf | _: TExpLetIn
                | _: TExpMatchRow | _: TExpSeq) =>
      super.apply(isTail, exp)
  }
}

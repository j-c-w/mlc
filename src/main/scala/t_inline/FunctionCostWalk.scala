package t_inline

import scala.collection.mutable.HashMap
import tir._
import tpass.TPass

/* This walk goes over the tree and costs it.  */

object FunctionCostWalk
    extends TPass[HashMap[TInternalIdentVar, InlineReason] , Int] {
  override def default = 1
  override def combine(x: Int, y: Int) = x + y + 1

  // We make an assumpion that things that take lists
  // are really constant time overhead.
  override def combineList(ls: List[Int]) =
    ls.foldRight(1) ((x, y) => x + y)

  override def apply(internalApp: HashMap[TInternalIdentVar, InlineReason],
                     exp: TExp) = exp match {
    // If this application is going to be inlined, then we return the new
    // inlined cost rather than the old cost.
    case TExpFunApp(fun, app, typ) => {
      internalApp(typ) match {
        case Inline(cost, thresh, funName, applicationTuple: List[TExp],
                    _, _) =>
          // We want the cost of the expressions being applied, not whatever
          // exists in this node.  Add 1 for the application cost.
          cost + combineList(applicationTuple.map(apply(internalApp, _))) + 1
        case other =>
          super.apply(internalApp, exp)
      }
    }
    // We MUST NOT inline exception handlers.  This is only true for the JVM
    // and can be changed for other targets.
    case TExpTry(_, _, _, _) =>
      10000
    case TExpHandle(_, _, _) =>
      10000
    case other => super.apply(internalApp, exp)
  }

  override def apply(internalApp: HashMap[TInternalIdentVar, InlineReason],
                     dec: TDec) = dec match {
    case TFun(name, List(TExpMatchRow(pats, exp, env))) =>
      // This is a function with a single pattern.
      // The pattern can almost certainly later be simplified, so
      // we do not count the cost of the match part:
      apply(internalApp, exp)
    case other =>
      super.apply(internalApp, other)
  }
}

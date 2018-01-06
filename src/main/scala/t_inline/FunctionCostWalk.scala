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
    case other => super.apply(internalApp, exp)
  }
}

package recursion_analysis

import function_analysis.FunctionAnalysis
import tir._

class IsTailRecursiveWalk(funName: TNamedIdent, topEnv: TTypeEnv,
                          curriedLength: Int)
    extends TParentSetTailPass {
  // A non-recursive function is tail recursive, but not usefully.
  var seenRecursiveCall = false
  var allTailCalls = true

  override def apply(isTail: Boolean, exp: TExp) = exp match {
    case funApp: TExpFunApp => {
      if(FunctionAnalysis.getFullApplicatonFrom(funName, curriedLength,
                                                topEnv, exp) != None) {
        allTailCalls = isTail && allTailCalls
        seenRecursiveCall = true
      }

      super.apply(isTail, funApp)
    }
    case other => super.apply(isTail, other)
  }
}

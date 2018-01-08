package recursion_analysis

import tir._

object RecursionAnalysis {
  /* Returns true if the function is tail recursive.  False otherwise.  */
  def isTailRecursive(fun: TFun, env: TTypeEnv) = {
    val walk =
      new IsTailRecursiveWalk(fun.name, env, fun.patterns(0).pat.length)
    walk.apply(true, fun)
    walk.seenRecursiveCall && walk.allTailCalls
  }
}

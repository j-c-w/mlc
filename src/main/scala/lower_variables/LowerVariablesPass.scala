package lower_variables

import toplev.Pass
import tir._

object LowerVariablesPass
    extends Pass[TJavaProgram, TJavaProgram]("lower_variables") {
  def numberVariables(fun: TJavaFun) = {
    new NumberVariablesWalk().apply((), fun)
  }

  def run(tree: TJavaProgram) = {
    numberVariables(tree.main)
    tree.functions.foreach(numberVariables(_))
    tree
  }
}

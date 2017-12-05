package lambda_lift

import toplev.Pass
import tir._

object LambdaLift extends Pass[TProgram, TProgram]("lambda_lift") {
  def run(tree: TProgram) = {
    val walk = new LambdaLiftWalk(tree)
    walk(tree.typeEnv, tree)

    walk.newTopLevelFunctions.foreach(
      UniqueifyVariablesWalk.uniqueify(walk.introducedVariables, _))
    tree
  }
}

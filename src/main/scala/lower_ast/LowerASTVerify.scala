package lower_ast

import toplev.OptionalPass
import tir.TProgram
import environment_soundness.EnvironmentSoundnessWalk

object LowerASTVerify extends OptionalPass[TProgram]("lower_ast_verify") {
  def run(tree: TProgram) = {
    val walk = new EnvironmentSoundnessWalk(tree.typeEnv)
    walk(tree.typeEnv, tree)
    tree
  }
}

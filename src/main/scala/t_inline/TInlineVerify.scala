package t_inline

import environment_soundness.EnvironmentSoundnessWalk
import tir._
import toplev.OptionalPass

object TInlineVerify extends OptionalPass[TProgram]("t_inline_verify") {
  def run(tree: TProgram) = {
    val walk = new EnvironmentSoundnessWalk(tree.typeEnv)
    walk(tree.typeEnv, tree)
    tree
  }
}

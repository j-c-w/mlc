package environment_soundness

import tir._
import toplev.OptionalPass

object ProgramEnvironmnetSoundness
    extends OptionalPass[TProgram]("environment_soundness") {
  def run(tree: TProgram) = {
    val walk = new EnvironmentSoundnessWalk(tree.typeEnv)
    walk(tree.typeEnv, tree)
    tree
  }
}

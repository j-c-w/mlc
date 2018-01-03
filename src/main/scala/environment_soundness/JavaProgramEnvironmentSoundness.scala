package environment_soundness

import tir._
import toplev.OptionalPass

object JavaProgramEnvironmnetSoundness
    extends OptionalPass[TJavaProgram]("environment_soundness") {
  def run(tree: TJavaProgram) = {
    val walk = new EnvironmentSoundnessWalk(tree.typeEnv)
    walk(tree.typeEnv, tree)
    tree
  }
}

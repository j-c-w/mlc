package environment_soundness

import tir._
import toplev.OptionalPass

object JavaProgramEnvironmnetSoundness
    extends OptionalPass[TJavaProgram]("environment_soundness") {
  def run(tree: TJavaProgram) = {
    EnvironmentSoundnessWalk(tree.typeEnv, tree)
    tree
  }
}

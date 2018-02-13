package t_outline

import toplev.Pass
import tir._

object TOutline extends Pass[TProgram, TProgram]("t_outline") {
  def run(program: TProgram) = {
    val walk = new OutlineWalk()
    walk.apply(program.typeEnv, program)
    program
  }
}

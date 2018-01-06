package simplify

import tir._
import toplev.OptionalPass

object PreLowerSimplify extends OptionalPass[TProgram]("pre_lower_simplify") {
  def run(tree: TProgram) = {
    CaseWalk((), tree)
    tree
  }
}

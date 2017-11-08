package change_names

import toplev.Pass
import tir._

object ChangeNames extends Pass[TProgramOrdered, TProgram]("change_names") {
  def run(tree: TProgramOrdered): TProgram = {
    tree.walk(new ChangeNamesWalk())

    // After the tree has been walked, the order of functions
    // is irrelevant. Sort through the decs and funs and put them
    // into lists.
    val unorderedProgram = TProgram(tree.typeEnv, List(), List())
    for (dec <- tree.decs) {
      dec match {
        case fun @ TFun(_, _) =>
          unorderedProgram.funs = fun :: unorderedProgram.funs
        case dec @ TVal(_, _) =>
          unorderedProgram.vals = dec :: unorderedProgram.vals
      }
    }

    // Finally, we have to reverse the lists
    unorderedProgram.funs = unorderedProgram.funs.reverse
    unorderedProgram.vals = unorderedProgram.vals.reverse

    unorderedProgram
  }
}

package t_inline

import scala.collection.mutable.HashSet
import tir._
import toplev.OptionalPass

object TInline extends OptionalPass[TProgram]("t_inline") {
  override def run(tree: TProgram) = {
    // Walk the tree and identify which functions are small enough  to inline.
    val costing = new CostingWalk(tree.typeEnv, tree)
    costing.apply(None, tree)

    // Then do the replacements
    val replacementWalk = new InlineWalk(tree, costing.shouldInlineMap,
                                         dumpString)
    replacementWalk.apply(tree.typeEnv, tree)

    // Append the number of dumps to the dump contents
    dumpString("Number of inlines = " + replacementWalk.inlineCount)

    // Then return the tree
    tree
  }
}

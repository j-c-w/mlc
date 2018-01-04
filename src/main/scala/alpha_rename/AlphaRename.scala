package alpha_rename

import tir._

/* This class alpha renames all elements of a tree.
 *
 * So, given some arbitrary tree which binds
 * variables a, b, c, ...
 * This returns an identical tree with variables
 * a', b', c'.
 *
 * The new variables are given the same types as
 * the old variables.
 */

object AlphaRename {
  def rename(tree: TExp, env: TTypeEnv) = {
    val alphaRenameWalk = new AlphaRenameWalk()
    alphaRenameWalk.apply(env, tree)

    val alphaApplyWalk = new AlphaApplyWalk(alphaRenameWalk.renameMap)
    alphaApplyWalk((), tree) match {
      case Some(newTree) => newTree
      case None => tree
    }
  }
}

package def_finder

import tir._

/* This is left as a shell implementation in the hope that it can be
 * made faster by caching results.
 *
 * The tough problem is figuring out how to invalidate cached
 * results.
 */

object DefFinder {
  def apply(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    new DefFinderWalk(ident).apply(env, program)
  }
}

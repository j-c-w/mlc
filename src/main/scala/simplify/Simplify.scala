package simplify

import tir._
import toplev.OptionalPass

/* This is not an optional pass.  The optimizations are basically just
 * cleanups, and the JVM expects them to be there.  */

object Simplify extends OptionalPass[TJavaProgram]("simplify") {
  def run(tree: TJavaProgram) = {
    TautologyWalk((), tree)
    tree
  }
}

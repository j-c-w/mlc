package t_typecheck

import tir._
import scala.collection.mutable.HashMap

object TTypecheck {
  /* Given some row pattern, and some function type, this specializes all the
   * types in the row pattern to that application type.
   *
   * e.g. if we are given the pattern:
   *    x => f x
   *  with type
   *    'a -> 'b
   * 
   * And requested to specialze it to int -> int, then this will go through
   * and:
   *  (a) update the types for 'x'
   *  (b) update the internal ident types.
   *
   * Note that no soundness checks are made.
   */
  def specialzeRowPattern(row: TExpMatchRow, appType: TFunctionType) = {
    val walk = new SpecializeRowPatternWalk(new HashMap())
    walk(appType, row)
    row
  }
}

package tir

import tpass.TPass

/* 
 * This trait contains the functions that a class
 * needs to implement to be walkable.
 *
 * walk(TPass) is provided as a function that should
 * walk the entire tree, including any replaced
 * nodes.
 */
trait TWalkable {
  def walk[T](item: T, f: TPass[T]): Unit
}

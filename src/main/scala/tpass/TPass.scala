package tpass

import tir._

/*
 * This is an interface for TreeIR passes.
 *
 * An object/class implementing this should then
 * be able to call TIR.walk(this) and have
 * the function defined called on every node.
 *
 * This abstracts away a large amount of the boilerplate
 * descent code.
 *
 * Each should return true to continue walking down the tree
 * and false to stop walking down the tree.
 */

trait TPass[T] {
  def apply(item: T, p: TConst): Boolean = true
  def apply(item: T, p: TExp): Boolean = true
  def apply(item: T, p: TFun): Boolean = true
  def apply(item: T, p: TIdent): Boolean = true
  def apply(item: T, p: TPat): Boolean = true
  def apply(item: T, p: TType): Boolean = true
  def apply(item: T, p: TVal): Boolean = true
  def apply(item: T, p: TProgram): Boolean = true
  def apply(item: T, p: TProgramOrdered): Boolean = true
}

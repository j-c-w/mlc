package tir

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

trait TPass {
  def apply(p: TConst): Boolean = true
  def apply(p: TExp): Boolean = true
  def apply(p: TFun): Boolean = true
  def apply(p: TIdent): Boolean = true
  def apply(p: TPat): Boolean = true
  def apply(p: TProgram): Boolean = true
  def apply(p: TType): Boolean = true
  def apply(p: TVal): Boolean = true
}

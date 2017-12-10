package tir

sealed trait TIdentClass extends TTree {
  def nodeClone: TIdentClass

  /* This should be true if the type can be put into a register or variable.
   *
   * e.g. for functions it is false, since they represent something more
   * abstract (like code).
   */
  def isRegisterClass: Boolean
}

case class TValClass() extends TIdentClass {
  def prettyPrint = "_val"

  def isRegisterClass = true

  def nodeClone = new TValClass()
}

case class TFunClass() extends TIdentClass {
  def prettyPrint = "_fun"

  def isRegisterClass = false

  def nodeClone = new TFunClass()
}

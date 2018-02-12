package tir

sealed trait TIdentClass extends TTree {
  def nodeClone(env: TTypeEnv): TIdentClass

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

  def nodeClone(env: TTypeEnv) = new TValClass()
}

case class TFunClass() extends TIdentClass {
  def prettyPrint = "_fun"

  def isRegisterClass = false

  def nodeClone(env: TTypeEnv) = new TFunClass()
}

case class TDataTypeClass() extends TIdentClass {
  def prettyPrint = "_datatype"

  def isRegisterClass = false

  def nodeClone(env: TTypeEnv) = new TDataTypeClass()
}

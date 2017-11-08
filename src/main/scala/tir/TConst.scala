package tir

import toplev.GenericPrintable

sealed trait TConst extends TWalkable with GenericPrintable

case class TConstInt(val int: Int) extends TConst {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = Integer.toString(int)
}
case class TConstFloat(val float: Double) extends TConst {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = String.valueOf(float)
}

case class TConstString(val str: String) extends TConst {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = "\"" + str + "\""
}

case class TConstChar(val char: Char) extends TConst {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = Character.toString(char)
}

sealed trait TConstBool extends TConst

case class TConstTrue() extends TConstBool {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = "true"
}

case class TConstFalse() extends TConstBool {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = "false"
}

package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TConst extends TTree {
  def nodeClone: TConst
}

case class TConstInt(val int: Int) extends TConst {
  def prettyPrint = Integer.toString(int)

  def nodeClone = new TConstInt(int)
}
case class TConstFloat(val float: Float) extends TConst {
  def prettyPrint = String.valueOf(float)

  def nodeClone = new TConstFloat(float)
}

case class TConstString(val str: String) extends TConst {
  def prettyPrint = "\"" + str + "\""

  def nodeClone = new TConstString(new String(str))
}

case class TConstChar(val char: Char) extends TConst {
  def prettyPrint = Character.toString(char)

  def nodeClone = new TConstChar(char)
}

sealed trait TConstBool extends TConst

case class TConstTrue() extends TConstBool {
  def prettyPrint = "true"

  def nodeClone = new TConstTrue()
}

case class TConstFalse() extends TConstBool {
  def prettyPrint = "false"

  def nodeClone = new TConstFalse()
}

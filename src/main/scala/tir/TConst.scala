package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TConst extends TTree

case class TConstInt(val int: Int) extends TConst {
  def prettyPrint = Integer.toString(int)
}
case class TConstFloat(val float: Double) extends TConst {
  def prettyPrint = String.valueOf(float)
}

case class TConstString(val str: String) extends TConst {
  def prettyPrint = "\"" + str + "\""
}

case class TConstChar(val char: Char) extends TConst {
  def prettyPrint = Character.toString(char)
}

sealed trait TConstBool extends TConst

case class TConstTrue() extends TConstBool {
  def prettyPrint = "true"
}

case class TConstFalse() extends TConstBool {
  def prettyPrint = "false"
}

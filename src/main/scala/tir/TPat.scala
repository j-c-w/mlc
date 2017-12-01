package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TPat extends TTree

case class TPatWildcard() extends TPat {
  def prettyPrint = "_"
}

case class TPatVariable(var variable: TIdentVar) extends TPat {
  def prettyPrint = variable.prettyPrint
}

case class TPatIdentifier(var identifier: TIdent) extends TPat {
  def prettyPrint = identifier.prettyPrint
}

case class TPatSeq(var seq: List[TPat]) extends TPat {
  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def prettyPrint = "[" + listElems.mkString(", ") + "]"
}

case class TPatConst(var const: TConst) extends TPat {
  def prettyPrint = const.prettyPrint
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)
}

package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TPat extends TTree {
  def nodeClone: TPat
}

case class TPatWildcard() extends TPat {
  def prettyPrint = "_"

  def nodeClone = new TPatWildcard()
}

case class TPatVariable(var variable: TIdentVar) extends TPat {
  def prettyPrint = variable.prettyPrint

  def nodeClone = new TPatVariable(variable.nodeClone)
}

case class TPatIdentifier(var identifier: TIdent) extends TPat {
  def prettyPrint = identifier.prettyPrint

  def nodeClone = new TPatIdentifier(identifier.nodeClone)
}

case class TPatSeq(var seq: List[TPat]) extends TPat {
  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone = new TPatSeq(seq.map(_.nodeClone))
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def prettyPrint = "[" + listElems.mkString(", ") + "]"

  def nodeClone = new TListPat(listElems.map(_.nodeClone))
}

case class TPatConst(var const: TConst) extends TPat {
  def prettyPrint = const.prettyPrint

  def nodeClone = new TPatConst(const.nodeClone)
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)

  def nodeClone = new TPatCons(head.nodeClone, tail.nodeClone)
}

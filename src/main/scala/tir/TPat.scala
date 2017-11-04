package tir

import toplev.GenericPrintable

trait TPat extends TWalkable with GenericPrintable

case class TPatWildcard() extends TPat {
  def walk(f: TPass) = f(this)

  def prettyPrint = "_"
}

case class TPatVariable(var variable: TIdent) extends TPat {
  def walk(f: TPass) = if (f(this)) {
    variable.walk(f)
  }

  def prettyPrint = variable.prettyPrint
}

case class TPatSeq(var seq: List[TPat]) extends TPat {
  def walk(f: TPass) = if (f(this)) {
    seq.foreach(_.walk(f))
  }

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def walk(f: TPass) = if (f(this)) {
    listElems.foreach(_.walk(f))
  }

  def prettyPrint = "[" + listElems.mkString(", ") + "]"
}

case class TPatConst(var const: TConst) extends TPat {
  def walk(f: TPass) = if (f(this)) {
    const.walk(f)
  }

  def prettyPrint = const.prettyPrint
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def walk(f: TPass) = if (f(this)) {
    head.walk(f)
    tail.walk(f)
  }

  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)
}

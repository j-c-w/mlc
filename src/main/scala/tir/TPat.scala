package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TPat extends TWalkable with GenericPrintable

case class TPatWildcard() extends TPat {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "_"
}

case class TPatVariable(var variable: TIdentVar) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    variable.walk(item, f)
  }

  def prettyPrint = variable.prettyPrint
}

case class TPatIdentifier(var identifier: TIdent) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    identifier.walk(item, f)
  }

  def prettyPrint = identifier.prettyPrint
}

case class TPatSeq(var seq: List[TPat]) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    seq.foreach(_.walk(item, f))
  }

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    listElems.foreach(_.walk(item, f))
  }

  def prettyPrint = "[" + listElems.mkString(", ") + "]"
}

case class TPatConst(var const: TConst) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    const.walk(item, f)
  }

  def prettyPrint = const.prettyPrint
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    head.walk(item, f)
    tail.walk(item, f)
  }

  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)
}

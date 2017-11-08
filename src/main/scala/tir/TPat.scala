package tir

import toplev.GenericPrintable

sealed trait TPat extends TWalkable with GenericPrintable

case class TPatWildcard() extends TPat {
  def walk(env: TTypeEnv, f: TPass) = f(env, this)

  def prettyPrint = "_"
}

case class TPatVariable(var variable: TIdentVar) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    variable.walk(env, f)
  }

  def prettyPrint = variable.prettyPrint
}

case class TPatIdentifier(var identifier: TIdent) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    identifier.walk(env, f)
  }

  def prettyPrint = identifier.prettyPrint
}

case class TPatSeq(var seq: List[TPat]) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    seq.foreach(_.walk(env, f))
  }

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    listElems.foreach(_.walk(env, f))
  }

  def prettyPrint = "[" + listElems.mkString(", ") + "]"
}

case class TPatConst(var const: TConst) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    const.walk(env, f)
  }

  def prettyPrint = const.prettyPrint
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def walk(env: TTypeEnv, f: TPass) = if (f(env, this)) {
    head.walk(env, f)
    tail.walk(env, f)
  }

  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)
}

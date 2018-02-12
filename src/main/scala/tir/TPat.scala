package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TPat extends TTree {
  def nodeClone(env: TTypeEnv): TPat
}

case class TPatWildcard() extends TPat {
  def prettyPrint = "_"

  def nodeClone(env: TTypeEnv) = new TPatWildcard()
}

case class TPatVariable(var variable: TNamedIdent) extends TPat {
  def prettyPrint = variable.prettyPrint

  def nodeClone(env: TTypeEnv) = new TPatVariable(variable.nodeClone(env))
}

case class TPatIdentifier(var identifier: TIdent) extends TPat {
  def prettyPrint = identifier.prettyPrint

  def nodeClone(env: TTypeEnv) = new TPatIdentifier(identifier.nodeClone(env))
}

case class TPatSeq(var seq: List[TPat]) extends TPat with TFlattenable[TPat] {
  assert(seq.length > 1)

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone(env: TTypeEnv) = new TPatSeq(seq.map(_.nodeClone(env)))

  def flatten = if (seq.length == 1)
    seq(0) match {
      case flattenable: TFlattenable[TPat] @unchecked => flattenable.flatten
      case other => other
    }
  else
    this
}

case class TListPat(var listElems: List[TPat]) extends TPat {
  def prettyPrint = "[" + listElems.mkString(", ") + "]"

  def nodeClone(env: TTypeEnv) = new TListPat(listElems.map(_.nodeClone(env)))
}

case class TPatConst(var const: TConst) extends TPat {
  def prettyPrint = const.prettyPrint

  def nodeClone(env: TTypeEnv) = new TPatConst(const.nodeClone(env))
}

case class TPatCons(var head: TPat, var tail: TPat) extends TPat {
  def prettyPrint = " (%s :: %s) ".format(head.prettyPrint, tail.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TPatCons(head.nodeClone(env), tail.nodeClone(env))
}

case class TPatConstructor(var typeName: TIdent, var typeArgs: Option[TPat])
    extends TPat {
  def prettyPrint = typeArgs match {
    case Some(typeArgs) =>
      "%s (%s)".format(typeName.prettyPrint, typeArgs.prettyPrint)
    case None => typeName.prettyPrint
  }

  def nodeClone(env: TTypeEnv) =
    new TPatConstructor(typeName.nodeClone(env),
                        typeArgs.map(_.nodeClone(env)))
}

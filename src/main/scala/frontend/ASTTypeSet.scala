package frontend

import toplev.TypeClassSet
import scala.collection.mutable.HashSet

object ASTTypeSet {
  def apply() = new ASTTypeSet()
  def apply(typ: ASTType) = {
    val s = new ASTTypeSet()
    s.insert(typ)
    s
  }
}

class ASTTypeSet(val set: HashSet[ASTType]) extends TypeClassSet[ASTType] {
  def this() = this(new HashSet[ASTType]())

  def in(typ: ASTType) = set.contains(typ)

  def union(other: TypeClassSet[ASTType]) = {
    other.getMembers().foreach(x => insert(x))
    this
  }

  def insert(typ: ASTType) = {
    set += typ
    this
  }

  def getMembers = set.toList

  def filter(f: ASTType => Boolean) = new ASTTypeSet(set.filter(f))

  def foreach(f: ASTType => Unit) = set.foreach(f)

  def size = set.size

  def prettyPrint =
    "(" + getMembers().map(_.prettyPrint).mkString(", ") + ")"
}

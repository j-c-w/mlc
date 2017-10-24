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

class ASTTypeSet() extends TypeClassSet[ASTType] {
  val set = HashSet[ASTType]()

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

  def filter(f: ASTType => Boolean) = {
    set.filter(f)
    this
  }
  def foreach(f: ASTType => Unit) = {
    set.foreach(f)
    this
  }
  def size = set.size
}

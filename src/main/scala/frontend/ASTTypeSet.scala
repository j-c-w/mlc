package frontend

import toplev.GenericTypeSet
import scala.collection.mutable.HashSet

object ASTTypeSet {
  def apply() = new ASTTypeSet()
  def apply(typ: ASTType) = {
    val s = new ASTTypeSet()
    s.insert(typ)
    s
  }
}

class ASTTypeSet(val set: HashSet[ASTType]) extends GenericTypeSet[ASTType] {
  def this() = this(new HashSet[ASTType]())

  def in(typ: ASTType) = set.contains(typ)

  def insert(typ: ASTType) = {
    set += typ
    this
  }

  def getMembers = set.toList

  def size = set.size

  def newSet = ASTTypeSet()
}

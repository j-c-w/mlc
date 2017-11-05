package tir

import scala.collection.mutable.HashSet
import toplev.TypeClassSet

class TTypeSet(val set: HashSet[TType]) extends TypeClassSet[TType] {
  def this() = this(new HashSet[TType]())

  def in(typ: TType) = set.contains(typ)

  def insert(typ: TType) = {
    set += typ
    this
  }

  def getMembers = set.toList

  def size = set.size

  def newSet = new TTypeSet()
}

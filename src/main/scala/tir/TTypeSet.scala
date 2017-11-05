package tir

import scala.collection.mutable.HashSet
import toplev.TypeClassSet

class TTypeSet(val set: HashSet[TType]) extends TypeClassSet[TType] {
  def this() = this(new HashSet[TType]())

  def union(other: TypeClassSet[TType]) = {
    other.getMembers().foreach(insert(_))
    this
  }

  def in(typ: TType) = set.contains(typ)

  def insert(typ: TType) = {
    set += typ
    this
  }

  def getMembers = set.toList

  def filter(f: TType => Boolean) = new TTypeSet(set.filter(f))

  def foreach(f: TType => Unit) = set.foreach(f)

  def size = set.size

  def -(other: TypeClassSet[TType]) = {
    val newSet = new TTypeSet()

    for (item <- set) {
      if (!other.in(item)) {
        newSet.insert(item)
      }
    }

    newSet
  }

  def prettyPrint = "(" + getMembers.map(_.prettyPrint).mkString(", ") + ")"
}

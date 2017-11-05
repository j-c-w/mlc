package toplev

abstract class TypeClassSet[TypeClass <: GenericPrintable]
    extends GenericPrintable {
  def map[U <: GenericPrintable](builder: Unit => TypeClassSet[U],
             f: TypeClass => U): TypeClassSet[U] = {
    val set = builder()
    foreach(item => set.insert(f(item)))
    set
  }

  def isEmpty: Boolean = (size == 0)

  def union(other: TypeClassSet[TypeClass]): TypeClassSet[TypeClass] = {
    val unionSet = newSet

    for (typ <- other.getMembers()) {
      unionSet.insert(typ)
    }

    for (typ <- this.getMembers()) {
      unionSet.insert(typ)
    }

    unionSet
  }

  def intersection(other: TypeClassSet[TypeClass]): TypeClassSet[TypeClass] = {
    val intersectionSet = newSet

    for (member <- getMembers()) {
      if (other.in(member)) {
        intersectionSet.insert(member)
      }
    }

    intersectionSet
  }

  def filter(f: (TypeClass) => Boolean): TypeClassSet[TypeClass] = {
    val filteredSet = newSet

    for (member <- getMembers()) {
      if (f(member)) {
        filteredSet.insert(member)
      }
    }

    filteredSet
  }

  def foreach(f: (TypeClass) => Unit): Unit = {
    getMembers.foreach(f)
  }

  /* Computes the difference between sets
   */
  def -(other: TypeClassSet[TypeClass]): TypeClassSet[TypeClass] = {
    val differenceSet = newSet

    for (member <- getMembers()) {
      if (!other.in(member)) {
        differenceSet.insert(member)
      }
    }

    differenceSet
  }

  def prettyPrint = "(" + getMembers().map(_.prettyPrint).mkString(", ") + ")"

  def size: Int

  def insert(typ: TypeClass): TypeClassSet[TypeClass]
  def getMembers(): List[TypeClass]
  def in(typ: TypeClass): Boolean
  def newSet: TypeClassSet[TypeClass]
}

package toplev

abstract class TypeClassSet[TypeClass] extends GenericPrintable {
  def map[U](builder: Unit => TypeClassSet[U],
             f: TypeClass => U): TypeClassSet[U] = {
    val set = builder()
    foreach(item => set.insert(f(item)))
    set
  }

  def union(other: TypeClassSet[TypeClass]): TypeClassSet[TypeClass]
  def in(typ: TypeClass): Boolean
  def insert(typ: TypeClass): TypeClassSet[TypeClass]
  def getMembers(): List[TypeClass]
  def filter(f: (TypeClass) => Boolean): TypeClassSet[TypeClass]
  def foreach(f: (TypeClass) => Unit): Unit
  def size: Int
  /* Computes the difference between sets
   */
  def -(other: TypeClassSet[TypeClass]): TypeClassSet[TypeClass]
}

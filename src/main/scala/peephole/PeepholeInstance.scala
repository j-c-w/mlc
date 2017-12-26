package peephole

import scala.collection.mutable.Set

trait PeepholeInstance[Instruction]
    extends Comparable[PeepholeInstance[Instruction]] {
  def getSize: Int

  def matches(input: List[Set[Instruction]]): Option[List[Instruction]]
}

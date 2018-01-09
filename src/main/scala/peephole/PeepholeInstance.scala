package peephole

trait PeepholeInstance[Instruction]
    extends Comparable[PeepholeInstance[Instruction]] {
  def getSize: Int

  def matches(input: List[Instruction]): Option[List[Instruction]]
}

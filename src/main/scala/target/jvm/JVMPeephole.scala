package target.jvm

import byteR._
import peephole.{PeepholeInstance,PeepholeSet}

object JVMPeephole {
  lazy val peepholes: Array[PeepholeInstance[JVMInstruction]] = Array(
    UnitPop,
    BoxUnbox,
    IntPushPop,
    BranchBoxUnbox
  )

  def getSet =
    new PeepholeSet(peepholes)
}

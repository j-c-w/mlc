package target.jvm

import byteR._
import peephole.{PeepholeInstance,PeepholeSet}

object JVMPeephole {
  lazy val peepholes: Array[PeepholeInstance[JVMInstruction]] = Array(
    UnitPop,
    BoxUnbox,
    IntPushPop,
    BranchBoxUnbox,
    ObjectCast
  )

  def getSet =
    new PeepholeSet(peepholes)
}

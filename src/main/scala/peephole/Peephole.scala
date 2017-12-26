package peephole

import byteR._
import toplev.Pass

object Peephole
    extends Pass[(PeepholeSet[JVMInstruction], JVMProgram),
                 JVMProgram]("peephole") {
  def walkFunction(peepholeSet: PeepholeSet[JVMInstruction],
                   function: JVMMethod) = {
    val walk = new PeepholeWalk(function.body)
    function.body = walk.walk(peepholeSet)
  }

  def walkClass(peepholeSet: PeepholeSet[JVMInstruction], jvmClass: JVMClass) =
    jvmClass.methods.foreach(walkFunction(peepholeSet, _))

  def run(peephole: (PeepholeSet[JVMInstruction], JVMProgram)) = {
    peephole._2.classes.foreach(walkClass(peephole._1, _))
    peephole._2
  }
}

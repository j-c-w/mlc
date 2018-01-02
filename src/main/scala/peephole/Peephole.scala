package peephole

import byteR._
import toplev.{OptionalPass,Shared}

object Peephole
    extends OptionalPass[JVMProgram]("peephole") {
  def walkFunction(peepholeSet: PeepholeSet[JVMInstruction],
                   function: JVMMethod) = {
    val walk = new PeepholeWalk(function.body)
    function.body = walk.walk(peepholeSet)
  }

  def walkClass(peepholeSet: PeepholeSet[JVMInstruction], jvmClass: JVMClass) =
    jvmClass.methods.foreach(walkFunction(peepholeSet, _))

  def run(program: JVMProgram) = {
    program.classes.foreach(walkClass(Shared.targetConfig.peepholeSet, _))
    program
  }
}

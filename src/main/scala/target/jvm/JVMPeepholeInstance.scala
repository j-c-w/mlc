package target.jvm

import byteR._
import peephole.PeepholeInstance

abstract class JVMPeepholeInstance extends PeepholeInstance[JVMInstruction] {
  def compareTo(other: PeepholeInstance[JVMInstruction]): Int = ???
}

/* Turns:
 *
 * new Unit
 * dup
 * invokespecial ...
 * pop
 *
 * into nothing.
 */
object UnitPop extends JVMPeepholeInstance {
  def getSize = 4

  def matches(input: List[JVMInstruction]) = {
    assert(input.length == 4)
    input match {
      case List(_: JVMNew, _: JVMDup, _: JVMInvokeSpecialMethod, _: JVMPop) =>
        Some(List())
      case _ => None
    }
  }
}

object IntPushPop extends JVMPeepholeInstance {
  def getSize = 3

  def matches(input: List[JVMInstruction]) = {
    assert(input.length == 3)

    input match {
      case List(JVMIPush(_),
                JVMInvokeStaticMethod(JVMMethodRef(_: JVMBoxedRef,
                                                   "valueOf",
                                                   List(_: JVMPrimitiveType),
                                                   _: JVMBoxedType)),
                _: JVMPop) =>
          Some(List())
      case _ => None
    }
  }
}

object BoxUnbox extends JVMPeepholeInstance {
  def getSize = 2

  def matches(input: List[JVMInstruction]) = {
    assert(input.length == 2)

    input match {
      case List(JVMInvokeStaticMethod(JVMMethodRef(_: JVMBoxedRef, "valueOf",
                                                  List(_: JVMPrimitiveType),
                                                  _: JVMBoxedType)),
                JVMInvokeVirtualMethod(JVMMethodRef(_: JVMBoxedRef, name,
                                                    List(),
                                                    _: JVMPrimitiveType))) =>
        if (name.endsWith("Value")) {
          Some(List())
        } else {
          None
        }
      case _ => None
    }
  }
}

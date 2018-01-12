package target.jvm

import byteR._
import peephole.PeepholeInstance

abstract class JVMPeepholeInstance extends PeepholeInstance[JVMInstruction] {
  def compareTo(other: PeepholeInstance[JVMInstruction]): Int = ???

  def isBoxUnboxPair(box: JVMInstruction, unbox: JVMInstruction) =
    (box, unbox) match {
      case (JVMInvokeStaticMethod(JVMMethodRef(_: JVMBoxedRef, "valueOf",
                                               List(_: JVMPrimitiveType),
                                                _: JVMBoxedType)),
            JVMInvokeVirtualMethod(JVMMethodRef(_: JVMBoxedRef, name,
                                                List(),
                                                _: JVMPrimitiveType))) =>
        if (name.endsWith("Value"))
          true
        else
          false
      case _ => false
  }
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
      case List(box, unbox) =>
        if (isBoxUnboxPair(box, unbox)) {
          Some(List())
        } else {
          None
        }
      case _ => None
    }
  }
}


/* This peephole can be deleted when code motion is implemented.  */
object BranchBoxUnbox extends JVMPeepholeInstance {
  def getSize = 9

  def matches(input: List[JVMInstruction]) = {
    assert(input.length == 5)

    input match {
      case List(cond: JVMConditionalJumpInstruction,
                loadInstruction1,
                boxInstruction1,
                endThenJump: JVMJumpInstruction,
                elseMark @ JVMLabelMark(elseLabel),
                loadInstruction2,
                boxInstruction2,
                endIfMark @ JVMLabelMark(endIfLabel),
                unboxInstruction) => {
        if (cond.getTarget == elseLabel
            && endThenJump.getTarget == endIfLabel
            && loadInstruction1.stackEffect == 1
            && loadInstruction2.stackEffect == 1
            && isBoxUnboxPair(boxInstruction1, unboxInstruction)
            && isBoxUnboxPair(boxInstruction2, unboxInstruction))
          Some(List(cond, loadInstruction1, endThenJump,
                    elseMark, loadInstruction2, endIfMark))
        else
          None
      }
    }
  }
}

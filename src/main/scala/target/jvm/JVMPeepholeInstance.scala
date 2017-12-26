package target.jvm

import byteR._
import peephole.PeepholeInstance
import scala.collection.mutable.Set

abstract class JVMPeepholeInstance extends PeepholeInstance[JVMInstruction] {
  def setMatch(set: Set[JVMInstruction],
               matchesFun: JVMInstruction => Boolean) = 
    set.forall(matchesFun)

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

  def matches(input: List[Set[JVMInstruction]]) = {
    assert(input.length == 4)
    input match {
      case List(i1, i2, i3, i4) =>
        if (setMatch(i1, _.isInstanceOf[JVMNew]) &&
            setMatch(i2, _.isInstanceOf[JVMDup]) &&
            setMatch(i3, _.isInstanceOf[JVMInvokeSpecialMethod]) &&
            setMatch(i4, _.isInstanceOf[JVMPop]))
          Some(List())
        else None
      case _ => None
    }
  }
}

object IntPushPop extends JVMPeepholeInstance {
  def getSize = 3

  def matches(input: List[Set[JVMInstruction]]) = {
    assert(input.length == 3)

    input match {
      case List(iload, box, pop) => {
        val iloadMatch = iload.forall {
          case JVMIPush(_) => true
          case _ => false
        }
        val boxMatch = box.forall {
          case JVMInvokeStaticMethod(JVMMethodRef(classRef, "valueOf",
                                                  List(arg), res)) =>
            classRef.isInstanceOf[JVMBoxedRef] &&
            arg.isInstanceOf[JVMPrimitiveType] &&
            res.isInstanceOf[JVMBoxedType]
          case _ => false
        }
        val popMatch = pop.forall {
          case JVMPop() => true
          case _ => false
        }

        if (iloadMatch && boxMatch && popMatch)
          Some(List())
        else
          None
      }
    }
  }
}

object BoxUnbox extends JVMPeepholeInstance {
  def getSize = 2

  def matches(input: List[Set[JVMInstruction]]) = {
    assert(input.length == 2)

    input match {
      case List(box, unbox) => {
        val boxMatch = box.forall {
          case JVMInvokeStaticMethod(JVMMethodRef(classRef, "valueOf",
                                                  List(arg), res)) =>
            classRef.isInstanceOf[JVMBoxedRef] &&
            arg.isInstanceOf[JVMPrimitiveType] &&
            res.isInstanceOf[JVMBoxedType]
          case other => false
        }
        val unboxMatch = unbox.forall {
          case JVMInvokeVirtualMethod(JVMMethodRef(classRef, name,
                                                   List(), res)) =>
            classRef.isInstanceOf[JVMBoxedRef] &&
            res.isInstanceOf[JVMPrimitiveType] &&
            // The name will be of the form: booleanValue, intValue etc.
            name.endsWith("Value")
          case other => false
        }

        if (boxMatch && unboxMatch) {
          Some(List())
        } else {
          None
        }
      }
    }
  }
}

package byteR.cfg

import byteR._
import exceptions.ICE
import scala.collection.mutable.{HashMap,HashSet}
import toplev.GenericPrintable


/* This is a structure for storing a CFG of the byteR representation.
 *
 * The jumpSet is as produced by the CFG building walk, a map of instruction
 * index at the start of a BB to the last instruction index in that BB
 * and the start indecies of any following BBs.
 */

class JVMCFG(val jumpSet: HashMap[BBStart, (BBEnd, BBPred, BBSucc)],
             val instructions: Array[JVMInstruction])
    extends GenericPrintable {
  def bbToString(from: BBStart) = {
    val (to, preds, next) = jumpSet(from)

    "\nBB Number " + from + "\n" +
    "BB Range " + from + "-" + to + "\n" +
    "Predecessors: " + preds.prettyPrint + "\n\n" +
    getBB(from, to).map(_.prettyPrint).mkString("\n") +
    "\n\nSuccessors BBs: " + next.prettyPrint
  }

  def getBB(from: BBStart): Array[JVMInstruction] = {
    val (end, _, _) = jumpSet(from)
    getBB(from, end)
  }

  def getBB(from: BBStart, to: BBEnd): Array[JVMInstruction] =
    instructions.slice(from.index, to.index + 1)

  /* This returns a set of indicies paired with instructions
   * that are defs in an instruction set.  */
  def getDefsIn(from: BBStart) = {
    val instructions = getBB(from)

    instructions.collect {
      case store: JVMStoreInstruction => store.getVariable
    }
  }

  def getDefsInSet(from: BBStart) =
    toHashSet(getDefsIn(from))

  def getRefsIn(from: BBStart) = {
    val instructions = getBB(from)

    instructions.collect {
      case load: JVMLoadInstruction => load.getVariable
    }
  }

  def getRefsInSet(from: BBStart) =
    toHashSet(getRefsIn(from))

  private def toHashSet(arr: Array[JVMVariable]) = {
    val set = new HashSet[JVMVariable]()
    set ++= arr
    set
  }

  def walk(bbFun: (BBStart, BBEnd, BBPred, BBSucc) => Unit) =
    jumpSet.foreach {
      case (from, (to, pred, succ)) => bbFun(from, to, pred, succ)
    }

  /* See description for the helper method.  */
  def walkWhileChanges(bbFun: (BBStart, BBEnd,
                               BBPred, BBSucc) => Boolean): Unit =
   walkWhileChanges(bbFun, 0)

  /* Given a function that takes:
   *
   *  ((Start, End), Following blocks)
   *
   * And returns true if there was a change in that BB, and false
   * if there was no change in that BB, walk the list of BBs
   * until no no new changes are reported.
   *
   * Terminate if there are more than 1,000,000 iterations of this function.
   */
  @scala.annotation.tailrec
  private def walkWhileChanges(bbFun: (BBStart, BBEnd,
                                       BBPred, BBSucc) => Boolean,
                               n: Int): Unit = {
    if (n == 1000000) {
      throw new ICE("BB Walk appears to be non-terminating")
    }

    if (jumpSet.exists {
        case (from, (to, pred, succ)) => bbFun(from, to, pred, succ)
      }) {
      walkWhileChanges(bbFun, n + 1)
    }
  }

  def prettyPrint = """
  |CFG:

  |%s
  """.stripMargin.format(jumpSet.map {
    case (from, (to, preds, next)) => bbToString(from)
  }.mkString("\n\n---- BB Divide ----\n"))
}

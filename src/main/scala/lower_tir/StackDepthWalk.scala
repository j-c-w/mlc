package lower_tir

import byteR._
import byte_walk._
import exceptions.ICE
import scala.collection.mutable.{HashMap,Queue}

/* This is required as part of the lowering.  Once optimizations that change
 * the stack depth are included, this should be re-run and the stack depth
 * updated appropriately.
 */
class StackDepthWalk(val instrs: List[JVMInstruction]) {
  // This map stores the depth at each instruction.
  // This pass also acts as a verification that each instruction has the same
  // depth on all paths to it.
  private val instructionDepths = new HashMap[Int, Int]()
  // println("New instructions")
  // println(instrs.mkString("\n"))
  private lazy val followSet: HashMap[Int, List[(Int, JVMInstruction)]] =
    BWalk.walk(instrs)

  // (Depth, (Instruction number, instruction))
  private val depthSearchQueue: Queue[(Int, (Int, JVMInstruction))] =
    new Queue[(Int, (Int, JVMInstruction))]()

  lazy val findDepth = {
    depthSearchQueue.enqueue((0, (0, instrs.head)))
    calculateDepth(0)

    // Then return the maximum element in the depths.
    max(instructionDepths)
  }

  private def max(set: HashMap[Int, Int]) = {
    // The stack depth cannot be less than 0.
    var currentMax = 0

    for ((_, depth) <- set) {
      if (depth > currentMax) {
        currentMax = depth
      }
    }

    currentMax
  }

  /* Since there could easily be over 2000 instructions, this function has
   * been modified from its intuitive form to be tail recursive.  */
  @scala.annotation.tailrec
  private def calculateDepth(maxIterations: Int): Unit = {
    if (maxIterations == 100000) {
      // This is here for safety.  It is really hard to find non-termination
      // bugs.
      throw new ICE("Stack depth calculation did not terminate. ")
    }

    if (depthSearchQueue.isEmpty) {
      // Termiante the walk if the queue is empty.
      return;
    }

    // Otherwise, get the head off of the queue.
    val (currentDepth, (number, instr)) = depthSearchQueue.dequeue()
    val nextInstrs = followSet(number)
    val newDepth = currentDepth + instr.stackEffect
    // println("Instruction was " + instr)
    // println(newDepth)
    // println("Instruction number is " + number)
    assert(newDepth >= 0)

    if (instructionDepths.contains(number)) {
      // We have already walked this instruction, so just verify that it is
      // correct and stop.
      // println("This depth is " + newDepth)
      // println("Other depth is " + instructionDepths(number))
      assert(instructionDepths(number) == newDepth)
    } else {
      // For saftey, we assert that if this is a return statement, then the
      // stack  is empty.
      instr match {
        case ret: JVMReturn => assert(newDepth == 0)
        case other =>
      }
      instructionDepths(number) = newDepth

      // Add all the subinstructions to the queue to search for.
      followSet(number).foreach {
        case (instrNumber, instr) =>
          depthSearchQueue.enqueue((newDepth, (instrNumber, instr)))
      }
    }

    // And apply the continuation to continue the search.
    calculateDepth(maxIterations + 1)
  }
}

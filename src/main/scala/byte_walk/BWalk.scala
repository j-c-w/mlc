package byte_walk

import byteR._
import scala.collection.mutable.HashMap

/* This creates a hash table that, given an instruction, returns a list
 * of instructions that follow  immediately.
 *
 * Rather than hashing instructions, this hashes the instruction index
 * to keep everything unique.
 */

object BWalk {
  def walk(instructions: List[JVMInstruction]) = {
    // The idea is to use these sets to build up the information we need to
    // build the final set.  The internal set keeps track of int to int
    // locations.  The label locations keeps track of the indecies of
    // labels.  The label jumps keeps track of which instructions point to
    // which labels.
    val internalSet =
      new HashMap[Int, List[Int]]()
    val labelLocations =
      new HashMap[JVMLabel, Int]()
    val labelJumps =
      new HashMap[Int, JVMLabel]()

    (instructions zip (0 until instructions.length)).foreach {
      case (labelInstr: JVMLabelInstruction, index) => {
        // If this is the last instruction, do not link it to the 'next'
        // instruction.
        val directFollow =
          if (index + 1 < instructions.length) List(index + 1) else List[Int]()

        labelInstr match {
          case branch: JVMCompareAndJumpInstruction => {
            internalSet(index) = directFollow
            labelJumps(index) = branch.getTarget
          }
          case JVMJump(target) => {
            internalSet(index) = List()
            labelJumps(index) = target
          }
          case JVMLabelMark(label) => {
            internalSet(index) = directFollow
            labelLocations(label) = index
          }
        }
      }
      case (other, index) =>
        internalSet(index) =
          if (index + 1 < instructions.length) List(index + 1) else List[Int]()
    }

    // With that all accumulated, we can build the actual set.  Note that we
    // are not interested in the transitive closure of this set (as that would
    // be everything!) just the one-step relation.
    val followSet =
      new HashMap[Int, List[(Int, JVMInstruction)]]()
    (instructions zip (0 until instructions.length)).map {
      case (instruction, index) => {
        followSet(index) = List()

        internalSet(index).foreach {
          x => followSet(index) = (x, instructions(x)) :: followSet(index)
        }
        // And get the label jumps:
        if (labelJumps.contains(index)) {
          val jumpTarget = labelJumps(index)
          val jumpIndex = labelLocations(jumpTarget)
          followSet(index) =
            (jumpIndex, instructions(jumpIndex)) :: followSet(index)
        }
      }
    }

    followSet
  }
}

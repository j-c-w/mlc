package byte_walk

import byteR._
import scala.collection.mutable.HashMap

/* This creates a hash table that, given an instruction, returns a list
 * of instructions that follow immediately.
 *
 * Rather than hashing instructions, this hashes the instruction index
 * to keep everything unique.
 */

object BWalk {
  /* This method returns an array of the instructions that push the operands
   * for 'index'.  It does not handle complex control flow that may be
   * required for these instructions.  To do that, an implementation with
   * the walk result or with a CFG is needed.  This is sufficient for
   * most DCE.  */
  def getPushInstructionsFor(index: Int,
                             instructions: List[JVMInstruction]) = {
    val instructionsArray = instructions.toArray
    val targetInstruction = instructionsArray(index)

    // Get the stack effect.
    val targetStackEffect = targetInstruction.stackEffect
    assert(targetStackEffect < 0)

    // Range start may be None if the access is not supported.
    val rangeStart =
      internalPushFor(index - 1, instructionsArray, targetStackEffect)
    // -1 as we should not include the use instruction.
    rangeStart.map(instructionsArray.slice(_, index))
  }

  /* This keeps going backwards until the net stack effect needed
   * has been found.  */
  @scala.annotation.tailrec
  private def internalPushFor(index: Int, instructions: Array[JVMInstruction],
                              stackEffect: Int): Option[Int] =
    stackEffect match {
      // Note that this has a delayed effect in reaching zero.
      // We add one to make up for this.
      case 0 => Some(index + 1)
      case n => {
        // N > 0 implies that one instruction pushed many things on...
        // Not right for this situation.
        assert(n < 0)
        val thisInstruction = instructions(index)
        val thisStackEffect = thisInstruction.stackEffect

        // If the instruction is a label, then there can be multiple
        // preceeding instructions.  This case is currently not handled.
        // Really, any DCE that is hidden behind control flow should be
        // dealt with in TIR.
        thisInstruction match {
          case labelInstruction: JVMLabelInstruction =>
            None
          case otherInstruction =>
            internalPushFor(index - 1, instructions,
                            stackEffect + thisStackEffect)
        }
      }
    }

  def walk(instructions: List[JVMInstruction]) = {
    val instructionsLength = instructions.length
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

    (instructions zip (0 until instructionsLength)).foreach {
      case (labelInstr: JVMLabelInstruction, index) => {
        // If this is the last instruction, do not link it to the 'next'
        // instruction.
        val directFollow =
          if (index + 1 < instructionsLength) List(index + 1) else List[Int]()

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
          if (index + 1 < instructionsLength) List(index + 1) else List[Int]()
    }

    // Rather than walk the list to get the nth element repeatedly,
    // use a single walk to make an array:
    val instructionsArray = instructions.toArray

    // With that all accumulated, we can build the actual set.  Note that we
    // are not interested in the transitive closure of this set (as that would
    // be everything!) just the one-step relation.
    val followSet =
      new HashMap[Int, List[(Int, JVMInstruction)]]()
    (instructions zip (0 until instructionsLength)).map {
      case (ret: JVMReturn, index) =>
        // A return brings control out of the function immediately.
        followSet(index) = List()
      case (athrow: JVMAThrow, index) =>
        // According to the JVM specification, this will clear stack.
        // The soundness of this is asserted via assertions in the
        // StackDepthWalk handlers.
        followSet(index) = List()
      case (instruction, index) => {
        followSet(index) = List()

        internalSet(index).foreach {
          x => followSet(index) = (x, instructionsArray(x)) :: followSet(index)
        }
        // And get the label jumps:
        if (labelJumps.contains(index)) {
          val jumpTarget = labelJumps(index)
          val jumpIndex = labelLocations(jumpTarget)
          followSet(index) =
            (jumpIndex, instructionsArray(jumpIndex)) :: followSet(index)
        }
      }
    }

    followSet
  }
}

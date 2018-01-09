package peephole

import byteR._
import byte_walk._
import scala.collection.mutable.{HashMap,HashSet,Map,MutableList,Set}

/* This is actually a walk of the instruction set.  It is similar to a
 * sequential walk so that it considers N-tuples of instructions that
 * are all next to each other (along every code path).
 *
 * However, this is an ammendment to that walk.  Since it does not deal
 * with whole functions, we don't need any of the nesting.  The only special
 * cases to cover are those of jumps and labels.
 */
class PeepholeWalk(val instructions: List[JVMInstruction]) {
  // This returns a new list of instructions using the peepholes
  // specified.
  def walk(peepholes: PeepholeSet[JVMInstruction]): List[JVMInstruction] = {
    val seqUpdateSet = new InstructionSeqUpdateSet[JVMInstruction]()
    peepholes.walk(0, instructions.length) (getSequenceSet(_)) ({
      // Annon function number two: Update the instructions list for the
      // peepholes.  Also keep the jump tables updated.
      case update @ InstructionSeqUpdate(_, _, _) => {
        seqUpdateSet.insert(update)
      }
    })

    // Now apply the updates to the instructions list.
    seqUpdateSet.apply(instructions)
  }

  private def getSequenceSet(range: PeepholeRange) = range match {
    case range @ SequenceRequest(start, length) =>
      getInstructionsFrom((start until (start + length)).toList)
  }

  private def getInstructionsFrom(locations: List[Int]) =
    locations map (getInstructionFrom(_))

  private def getInstructionFrom(location: Int) =
    if (location < 0 || location >= instructions.length) JVMNoInstruction
    else instructions(location)
}

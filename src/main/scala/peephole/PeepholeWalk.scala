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
 *
 * This can be cleaned up a LOT.  See the note in PeepholeSet.
 */
class PeepholeWalk(val instructions: List[JVMInstruction]) {
  // Compute the jump indecies for each label.
  val jumpIndicies: Map[JVMJumpInstruction, Int] = computeJumpIndicies
  // Also compute indicies that jump to each instruction for each label.
  val sourceIndicies: Map[JVMLabel, List[Int]] = computeSourceIndicies

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
    // This returns a sequence starting from instruction 'start'
    // if it exists uniquely.  Otherwise, return nothing.
    //
    // The kinds we consider are:
    //
    //  Jump L1
    //  ...
    //  L1:
    //  ...
    //
    // Returns an instruction sequence that features a list of instructions
    // before L1 (all the instructions whose labels can reach L1)
    //
    // And:
    //
    // IfCmpEq L1
    // ...
    // L1:
    // ...
    //
    // Similarly.  This has the extra catch of needing to also do the
    // same thing for the instructions following the conditional branch.
    case range @ SequenceRequest(start, length) => {
      val expandedInstructionsSeq =
        List.fill(length) (new HashSet[JVMInstruction])
      var alternateInstructions: MutableList[List[JVMInstruction]] =
        MutableList.fill(length) (List())

      // Now walk forward and backward over the range using the
      // preceed and proceed sets.
      // 
      // The idea is that we start at the front, walk through.  Whenever you
      // add instructions before the current instruction, got back to that
      // point and start walking through again.
      var internalIndex = 0
      var listIndex = start

      while (internalIndex < length) {
        // Check if there are any alternate instructions to deal with
        // at this index:
        while (!alternateInstructions(internalIndex).isEmpty) {
          val instruction = alternateInstructions(internalIndex).head
          alternateInstructions(internalIndex) =
            alternateInstructions(internalIndex).tail

          // Add this instruction to the list:
          expandedInstructionsSeq(internalIndex).add(instruction)

          // And add any other instructions that this one might require:
          getAlternateInstructionsFor(instruction, internalIndex,
                                      length - internalIndex)
        }
        val thisInstruction = getInstructionFrom(listIndex)
        expandedInstructionsSeq(internalIndex).add(thisInstruction)

        // Then expand the instruction a single time and merge these
        // sets:
        val instructionSets =
          getAlternateInstructionsFor(thisInstruction, internalIndex,
                                      length - internalIndex)
        
        // For the sake of fewer union operations, this is returned as
        // an option.
        instructionSets.map {
          case sets => {
            assert(sets.length == expandedInstructionsSeq.length)

            (sets zip expandedInstructionsSeq).foreach {
              case (tempSet, expandedSet) => {
                expandedSet ++= tempSet
              }
            }
          }
        }

        // If this is a Jump, then update  the listIndex to the target:
        thisInstruction match {
          case jump @ JVMJump(target) =>
            listIndex = jumpIndicies(jump)
          case _ =>
            listIndex += 1
        }

        internalIndex += 1
      }

      expandedInstructionsSeq
    }
  }

  /* This returns a list of all the alternate instructions that might follow
   * an instruction.  So, for something like:
   *
   *  ...
   *  L1:
   *  ...
   *
   * This returns all the instructions that lead up to (but not including)
   * a jump to L1.
   */
  private def getAlternateInstructionsFor(instruction: JVMInstruction,
                                          distBefore: Int, distAfter: Int) =
    instruction match {
      case condJump: JVMConditionalJumpInstruction => {
        var targetLocations = List(jumpIndicies(condJump) + 1)

        // Now get the dist after worth of instructions and put
        // those in a list of sets.
        // The offset is because the first instruction is going to be
        // the label mark for the cond jump.  We do not want to record
        // that.
        Some((List.fill(distBefore + 1) (new HashSet())) ++
             ((0 until (distAfter - 1)).map {
          case _ => {
            val targetInstructions = getInstructionsFrom(targetLocations)
            val resSet = new HashSet[JVMInstruction]()
            resSet ++= targetInstructions

            // Now update the indicies.  Mapping somewhat complicated
            // by the need to track jumps.
            targetLocations = (targetLocations zip targetInstructions) map {
              case (location, jump @ JVMJump(_)) => jumpIndicies(jump)
              case (location, _) => location + 1
            }

            resSet
          }
        }).toList)
      }
      case mark @ JVMLabelMark(label) => {
        var targetLocations: List[Int] = sourceIndicies(label) map {
          // Subtract 1 so that the jump instruction is not counted.
          case i => i - 1
        }

        // Now, for each of those locations, get the N preceeding instructions.
        Some(((1 to distBefore).map {
          case _ => {
            val targetInstructions = getInstructionsFrom(targetLocations)
            val resSet = new HashSet[JVMInstruction]()
            resSet ++= targetInstructions

            // The mapping is complicated because we must ensure that jumps
            // are tracked.
            targetLocations = (targetLocations zip targetInstructions) map {
              case (location, jump @ JVMJump(_)) => jumpIndicies(jump)
              case (location, _) => location - 1
            }

            resSet
          }
        }.reverse.toList) ++
        (List.fill(distAfter) (new HashSet[JVMInstruction]())))
      }
      // the number of instructions added by this is still one.
      case JVMJump(_) => None
      case other => None
    }

  private def getInstructionsFrom(locations: List[Int]) =
    locations map (getInstructionFrom(_))

  private def getInstructionFrom(location: Int) =
    if (location < 0 || location >= instructions.length) JVMNoInstruction
    else instructions(location)

  private def computeJumpIndicies: Map[JVMJumpInstruction, Int] = {
    val jumpIndicies = new HashMap[JVMLabel, Int]()
    val jumpMap = new HashMap[JVMJumpInstruction, JVMLabel]()

    (instructions zip (0 until instructions.length)).foreach {
      case (label @ JVMLabelMark(mark), index) =>
        jumpIndicies(mark) = index
      case (jump : JVMJumpInstruction, index) =>
        jumpMap(jump) = jump.getTarget
      case _ => // Only add label marks to the jump table.
    }

    jumpMap map {
      case (jump, label) => (jump, jumpIndicies(label))
    }
  }

  private def computeSourceIndicies: Map[JVMLabel, List[Int]] = {
    val sourceIndicies = new HashMap[JVMLabel, List[Int]]()

    (instructions zip (0 until instructions.length)).foreach {
      case (jump : JVMJumpInstruction, index) =>
        if (sourceIndicies.contains(jump.getTarget)) {
          sourceIndicies(jump.getTarget) =
            index :: sourceIndicies(jump.getTarget)
        } else {
          sourceIndicies(jump.getTarget) = List(index)
        }
      case _ =>
    }

    sourceIndicies
  }
}

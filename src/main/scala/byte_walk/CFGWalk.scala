package byte_walk

import byteR._
import byteR.cfg._
import exceptions._
import scala.collection.mutable.{HashMap,HashSet,Map}

object CFGWalk {
  def apply(instructions: List[JVMInstruction]): JVMCFG = {
    // This represents the CFG.  It goes from source locations
    // that are at the start of a basic block to source locations
    // that are at the start of the BB's that follow.
    val cfgMap = new HashMap[BBStart, (BBEnd, BBPred, BBSucc)]()

    // Int is the instruction index of the BB visited.
    val visitedBBs = new HashSet[BBStart]()
    // The successors are easy to set as the map is built up.
    // The predecessors must be added after each BB has had
    // successors accumulated.
    var bbPreds = new HashMap[BBStart, BBPred]()

    // Create the map:
    val followMap = BWalk.walk(instructions)
    // Do a DFS of the BB's.  At each BB, keep track of the live variables
    // set.
    var bbList = List(BBStart(0))
    bbPreds(BBStart(0)) = new BBPred(List())

    // Stop the while loop from running forever.
    var loopCount = 0

    while (bbList.length != 0) {
      loopCount += 1
      if (loopCount == 1000000) {
        throw new ICE("CFGWalk running forever")
      }
      val currentBB = bbList.head
      bbList = bbList.tail

      if (!visitedBBs.contains(currentBB)) {
        val (bbEndIndex, next) = bbApply(currentBB,
                                         instructions(currentBB.index),
                                         followMap)
        bbList = bbList ::: next.succs

        // Add this block to the list of successors of BBs in the next set.
        next.succs.foreach {
          case nextBB => if (bbPreds.contains(nextBB)) {
            bbPreds(nextBB) = BBPred(currentBB :: bbPreds(nextBB).preds)
          } else {
            bbPreds(nextBB) = BBPred(List(currentBB))
          }
        }

        // Build the CFG:
        // The BBPred is later overwritten.
        cfgMap(currentBB) = (bbEndIndex, BBPred(List()), next)

        visitedBBs += currentBB
      }
    }

    // Now, set the predecessors for each BB.
    cfgMap.foreach {
      case (start, (end, pred, succs)) =>
        cfgMap(start) = (end, bbPreds(start), succs)
    }

    new JVMCFG(cfgMap, instructions.toArray)
  }

  /* Given some instruction that is the start of a basic block,
   * go through the basic block until we reach a symbol indicating the
   * end of the BB.
   *
   * Return the range that constitutes this BB.
   *
   * The BB is identified by the index of the first instruction in it.
   */
  def bbApply(instructionNo: BBStart, startInstruction: JVMInstruction,
              instructionMap: Map[Int, List[(Int, JVMInstruction)]]):
        (BBEnd, BBSucc) = {
    // Prevent the loop from  running forever.
    var loopCount = 0;
    // Keep track of the index of the last instruction we have looked at.
    var bbCurrIndex = instructionNo.index
    var bbEndInstruction = startInstruction
    // Keep track of label instructions, which signal the end of BBs
    var endOfBB = false;

    while (instructionMap(bbCurrIndex).length == 1
           && !endOfBB) {
      loopCount += 1
      if (loopCount == 100000)
        throw new ICE("BB identification appears to be running indefinitely")

      // Note that this pattern match is safe as the know that the length
      // of the list is 1.
      val (nextInd, instruction) = instructionMap(bbCurrIndex) match {
        case List((index, instr)) => (index, instr)
        case _ => throw new UnreachableException()
      }

      bbEndInstruction = instruction
      bbCurrIndex = nextInd

      instruction match {
        case labelInstruction: JVMLabelInstruction =>
          // This is where the BB terminates.  This might happen if we have
          // e.g. a label mark, which could be some other BB jumping in here.
          endOfBB = true
        case other =>
      }
    }

    val bbEnd = bbEndInstruction match {
      // If the end index is a label mark, we need the next BB to
      // start with the same label mark.
      case lMark : JVMLabelMark => BBEnd(bbCurrIndex - 1)
      case other => BBEnd(bbCurrIndex)
    }

    val nextInstructionNos = instructionMap(bbEnd.index) map {
      case (no, instr) => BBStart(no)
    }

    (bbEnd, BBSucc(nextInstructionNos))
  }
}

package peephole

import exceptions.ICE
import scala.collection.mutable.{HashSet,Set}
import toplev.GenericPrintable

/* This is a wrapper class that can create a set of peepholes.
 *
 * The peepholes are stored in a sorted order (sorted progressively
 * on the Nth instruction) so that they can be used quickly over
 * some sequence of instructions.
 *
 * Active bugs:
 *
 *  - This could be cleaned up a /lot/.  It was originally designed to also
 *    do code movement.  That has been scrapped, but this is still inefficient.
 *
 *  - My new view is that there should be code motion that changes things like:
 *
 *      if_cmpeq L1
 *      ...
 *      box_instruction
 *      goto L2:
 *      L1:
 *      ...
 *      box_instruction
 *      L2:
 *      ...
 *
 *    By removing the duplicate instruction, these peepholes will be able
 *    to function as required.
 */
class PeepholeSet[PeepholeInstruction <: GenericPrintable]
    (val peepholes: Array[PeepholeInstance[PeepholeInstruction]]) {
  val peepholeSizes: Set[Int] = new HashSet[Int]()
  peepholes.foreach {
    case peephole => peepholeSizes.add(peephole.getSize)
  }

  def walk(startIndex: Int, endIndex: Int)
          (seqFor: PeepholeRange => List[Set[PeepholeInstruction]])
          (onUpdate: InstructionSeqUpdate[PeepholeInstruction] => Unit) = {
    // Walk each size of peephole over the list:
    peepholeSizes.foreach {
      case size => (startIndex until endIndex - size) foreach {
        case index => {
          val peepholeSequence = seqFor(SequenceRequest(index, size))
          matchPeepholes(peepholeSequence).map {
            case newSeq =>
              onUpdate(new InstructionSeqUpdate(index, index + size, newSeq))
          }
        }
      }
    }
  }

  def matchPeepholes(instructionSeq: List[Set[PeepholeInstruction]]) = {
    val results = peepholes.map ({
      case peephole => {
        if (peephole.getSize == instructionSeq.length)
          peephole.matches(instructionSeq)
        else
          None
      }
    }).filter(_ != None)

    if (results.length == 0)
      None
    else if (results.length == 1)
      results(0)
    else
      throw new ICE("""Multiple peepholes matched the sequence (%s)""".
        format(instructionSeq.map(_.map(_.prettyPrint)).mkString("|")
          .mkString(", ")))
  }
}

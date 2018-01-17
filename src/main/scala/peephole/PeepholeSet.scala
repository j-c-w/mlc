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
 */
class PeepholeSet[PeepholeInstruction <: GenericPrintable]
    (val peepholes: Array[PeepholeInstance[PeepholeInstruction]]) {
  val peepholeSizes: Set[Int] = new HashSet[Int]()
  peepholes.foreach {
    case peephole => peepholeSizes.add(peephole.getSize)
  }

  def walk(startIndex: Int, endIndex: Int)
          (seqFor: PeepholeRange => List[PeepholeInstruction])
          (onUpdate: InstructionSeqUpdate[PeepholeInstruction] => Unit) = {
    // Walk each size of peephole over the list:
    peepholeSizes.foreach {
      case size => (startIndex to endIndex - size) foreach {
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

  def matchPeepholes(instructionSeq: List[PeepholeInstruction]) = {
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
        format(instructionSeq.map(_.prettyPrint).mkString(", ")))
  }
}

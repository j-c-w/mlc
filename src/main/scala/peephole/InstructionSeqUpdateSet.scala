package peephole

import exceptions.ICE

class InstructionSeqUpdateSet[Inst] {
  // This is a list stored in order: So that the changes at the
  // smallest indicies are first in the list.
  private var updates: List[InstructionSeqUpdate[Inst]] = Nil

  def insert(update: InstructionSeqUpdate[Inst]) = update match {
    case InstructionSeqUpdate(from, to, newInstructions) => {
      val i = updates.indexWhere({
        case other @ InstructionSeqUpdate(otherFrom, otherTo, _) => {
          // If they overlap, then throw an exception.  We don't know
          // which peephole to apply.
          if (from < otherTo && to > otherFrom)
            throw new ICE("Peephole sequences %s and %s overlap".format
              (update, other))

          otherFrom > from
        }
      })
      val (front, back) =
        if (i == -1)
          // -1 means no index was found.
          (updates, List[InstructionSeqUpdate[Inst]]())
        else
          updates.splitAt(i)

      updates = front ++ List(update) ++ back
    }
  }

  def apply(instructions: List[Inst]): List[Inst] = {
    // Assert that the updates are in the right order:
    if (updates.length > 0) {
      var oldUpdate = updates.head

      for (update <- updates.tail) {
        assert (oldUpdate.fromIndex < update.fromIndex)
        oldUpdate = update
      }
    }
    if (instructions.length == 0 || updates.length == 0)
      return instructions

    var skipLength = 0

    var newInstructions: List[Inst] = Nil
    var currentHead = instructions.head
    var currentTail = instructions.tail

    var substitutionHead = updates.head
    var substitutionTail = updates.tail

    for (i <- (0 until instructions.length)) {
      if (skipLength > 0) {
        skipLength -= 1
      } else {
        // Check if this is the start of the top peephole:
        if (i == substitutionHead.fromIndex) {
          // Then do the substitution:
          // This will get reversed
          newInstructions =
            substitutionHead.newInstructions.reverse ++ newInstructions

          assert(skipLength == 0)
          skipLength =
            (substitutionHead.toIndex - substitutionHead.fromIndex) - 1

          if (substitutionTail != Nil) {
            substitutionHead = substitutionTail.head
            substitutionTail = substitutionTail.tail
          }
        } else { // endif: i is the start of a replacement
          newInstructions = currentHead :: newInstructions
        }
      } // endelse: skiplength > 0

      // Since this is done at the end of the loop, it would be done too many
      // times if we let it happen for the last index.
      if (i != instructions.length - 1) {
        currentHead = currentTail.head
        currentTail = currentTail.tail
      }
    }

    return newInstructions.reverse
  }
}

package byte_dce

import byteR._
import byte_walk.BWalk
import scala.collection.mutable.Map

object StoreDeletion {
  def apply(storeStatusMap: Map[Int, StoreStatus], method: JVMMethod,
            dumpEnabled: Boolean, dumpFunction: String => Unit) = {
    // Note that we walk through the stores backwards so as to avoid
    // influencing the indices.
    var numberEliminated = 0

    storeStatusMap.toList.sortBy(_._1).reverse.foreach {
      case (index, LiveStore) =>
      case (index, DeadStore) => {
        val instruction = method.body(index)
        if (instruction.isInstanceOf[JVMLocalAStore]
            || instruction.isInstanceOf[JVMSelfStore]) {
          val pushInstructions = BWalk.getPushInstructionsFor(index, method.body)

          pushInstructions match {
            case Some(instructions) => {
              if (dumpEnabled) {
                dumpFunction("Detected dead store at index " + index + "\n")
                dumpFunction("Instructions " +
                             instructions.toList + "\n")
              }

              if (instructions.forall(!_.hasSideEffect)) {
                // No side effects, so delete the whole lot.
                if (dumpEnabled) {
                  dumpFunction(" were side-effectless.  Deleted store. \n")
                }

                method.body =
                  spliceOut(method.body, index - instructions.length,
                            index + 1)
                numberEliminated += 1
              } else {
                // Has side effects.  We could replace the store with a pop if
                // that is deemed to be better.
                if (dumpEnabled) {
                  dumpFunction(" had side-effects.  Did not delete store \n")
                }
              }
            }
            case None =>
              if (dumpEnabled) {
                dumpFunction("Store at index " + index + "\n")
                dumpFunction("Was too complicated to analyze the push for. \n")
                dumpFunction("It is dead, but not deleted.")
              }
          }
        } else {
          // We do not consider stores to member variables and static variables
          // as dead.  They might appear dead in a function, but are really not
          // dead.  They are picked up by the DCE because it is trivial to do
          // so.
          if (dumpEnabled) {
            dumpFunction("Store at index " + index + "\n")
            dumpFunction("Is not to a local variable \n")
            dumpFunction("So is not safe to eliminate.\n")
          }
        }
      }
    }

    dumpFunction("Number eliminated = " + numberEliminated)
  }

  private def spliceOut(instructions: List[JVMInstruction],
                        from: Int, to: Int) = {
    assert(from <= to)

    val (before, after) = instructions.splitAt(from)
    before ++ after.drop(to - from)
  }
}

package byte_dce

import byteR._
import byteR.cfg._
import byte_walk._
import scala.collection.mutable.{HashMap,HashSet,Map}

/* This class uses the byte walk to build the follow set
 * for each instruction.  Each store is noted in a map.
 * If a read is reached (on any branch) it is noted in the map
 * so the store is not deleted.
 *
 */

object DCEWalk {
  /* Note that this function assumes that instructions form a BB.  */
  def applyBB(instructions: Array[JVMInstruction]) = {
    // We have to walk the instructions backwards.  A set of live
    // ref and def set is returned for this BB.  Variables can
    // be in both the ref and def sets (if they are referenced
    // before definition) 

    val residualRefs = new HashSet[JVMVariable]()
    val defs = new HashSet[JVMVariable]()

    instructions.reverse.foreach {
      case load: JVMLoadInstruction => {
        residualRefs += load.getVariable
      }
      case store: JVMStoreInstruction => {
        val variable = store.getVariable
        if (residualRefs.contains(variable)) {
          residualRefs -= variable
        }

        defs += variable
      }
      case _ =>
    }

    (defs, residualRefs)
  }

  def apply(cfg: JVMCFG) = {
    // Store the variables that are live into a BB.
    val liveIn = new HashMap[BBStart, HashSet[JVMVariable]]()

    // Go through the BBs and calculate the variables that are live in
    // to each BB.  Calculate the variables that are stored to in each BB.
    cfg.walkWhileChanges({
      case (from, to, predBBs, nextBBs) => {
        // Now, go through the instructions backwards.  There are a few
        // cases: (1) STORE -> STORE (First store was dead, second store
        // is dependent on the live out information).  Kill set is set
        // for the variable.
        //
        // (2) STORE -> LOAD (Store is live.  Variable is not live in,
        // killSet of variable is set)
        //
        // (3) LOAD -> STORE (Variable is live in, store is live dependent
        // on information going out).
        val (defs, refs) =  applyBB(cfg.getBB(from))

        val followLiveSets = nextBBs.succs map {
          case bbNum =>
            if (liveIn.contains(bbNum)) {
              liveIn(bbNum)
            } else {
              val set = new HashSet[JVMVariable]()
              liveIn(bbNum) = set
              set
            }
        }

        var newSetCreated = false

        val newSet =
          (bigUnion(followLiveSets) -- defs).union(refs)
        val oldSet = if (liveIn.contains(from)) {
          liveIn(from)
        } else {
          val set = new HashSet[JVMVariable]()
          liveIn(from) = set
          newSetCreated = true
          set
        }

        // We return true if there were changes and the walk needs to continue.
        val updated = if (newSet == oldSet) {
          false
        } else {
          liveIn(from) = newSet
          true
        }

        updated || newSetCreated
      }
    })

    // Label particular stores with some status.  This can be acted upon by a
    // later walk capable of deciding whether variables are resident.
    val storeStatus = new HashMap[Int, StoreStatus]

    // Then, within each BB, go through and look at each store to see if it
    // is a store to a dead variable.  If so, delete it.
    cfg.walk({
      case (from, to, preds, nextBBs) => {
        val nextBBsLiveIn = nextBBs.succs.map(liveIn(_))
        val liveOut = bigUnion(nextBBsLiveIn)

        // Get the instructions:
        val bbInstructions = cfg.getBB(from, to)

        // Create a set of variables referenced within this BB
        // since the last def of that variable.
        val localRefs = new HashSet[JVMVariable]()
        // Create a set of variables defined within this BB
        val localDefs = new HashSet[JVMVariable]()

        // Now walk the instructions (backwards) to see if stores are live
        // or dead.
        ((from.index to to.index) zip bbInstructions).reverse.foreach {
          case (index, refIns: JVMLoadInstruction) =>
            localRefs += refIns.getVariable
          case (index, defIns: JVMStoreInstruction) => {
            val defVar = defIns.getVariable

            // Check if this has been referenced since the last def:
            if (localRefs.contains(defVar)) {
              // This variable is no longer referenced before it is
              // defined.
              localRefs -= defVar
              
              // In this case, the store is live.
              storeStatus(index) = LiveStore
            } else {
              // This variable has not been referenced.
              // In the case that it has already been def'ed in this
              // BB, it should be deleted.
              if (localDefs.contains(defVar)) {
                storeStatus(index) = DeadStore
              } else if (!liveOut.contains(defVar)) {
                // If this varaible is not live out, we can delete the store.
                storeStatus(index) = DeadStore
              } else {
                // This store must not already have been def'd and it must
                // be live out.  Therefore, it is live.
                storeStatus(index) = LiveStore
              }
            }

            localDefs += defIns.getVariable
          }
          case _ =>
        }
      }
    })

    storeStatus
  }

  private def bigUnion(sets: List[HashSet[JVMVariable]]):
      HashSet[JVMVariable] = {
    sets.foldLeft(new HashSet[JVMVariable]) {
      case (builder, newSet) => builder.union(newSet)
    }
  }
}

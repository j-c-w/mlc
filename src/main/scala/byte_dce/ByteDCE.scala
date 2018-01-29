package byte_dce

import byteR._
import byte_walk.CFGWalk
import toplev.OptionalPass

/* See the README for a full description of this pass.  */

object ByteDCE extends OptionalPass[JVMProgram]("byte_dce") {
  def methodDCE(method: JVMMethod) = {
    // Create the CFG for the method instructions:
    val cfg = CFGWalk(method.body)

    // Dump the CFG into the output file.
    dumpString("CFG For " + method.name + " __ START __ \n\n")
    dumpString(cfg.prettyPrint)
    dumpString("CFG For " + method.name + " __ END __\n\n")

    // The stores map maps each store to a StoreStatus.
    val storesMap = DCEWalk.apply(cfg)

    dumpString("Store Statuses")
    dumpString(storesMap.toString)

    // Finally, delete any stores we deem it to be safe to do so.
    StoreDeletion.apply(storesMap, method, dumpString)
  }

  def run(tree: JVMProgram) = {
    tree.classes.foreach((javaClass) =>
        javaClass.methods.map(methodDCE(_)))
    tree
  }
}

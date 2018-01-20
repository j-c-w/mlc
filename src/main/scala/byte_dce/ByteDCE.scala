package byte_dce

import byteR._
import byte_walk.CFGWalk
import toplev.OptionalPass

/* See the README for a full description of this pass.  */

object ByteDCE extends OptionalPass[JVMProgram]("byte_dce") {
  var dumpString = new StringBuilder("")

  override def treeToString(tree: JVMProgram) = {
    """
    |%s
    |
    | Result code:
    |%s""".stripMargin.format(dumpString.toString(), tree.prettyPrint)
  }

  def methodDCE(method: JVMMethod) = {
    // Create the CFG for the method instructions:
    val cfg = CFGWalk(method.body)

    if (dumpEnabled) {
      // Dump the CFG into the output file.
      dumpString.append("CFG For " + method.name + " __ START __ \n\n")
      dumpString.append(cfg.prettyPrint)
      dumpString.append("CFG For " + method.name + " __ END __\n\n")
    }

    // The stores map maps each store to a StoreStatus.
    val storesMap = DCEWalk.apply(cfg)

    if (dumpEnabled) {
      dumpString.append("Store Statuses")
      dumpString.append(storesMap.toString)
    }

    // Finally, delete any stores we deem it to be safe to do so.
    StoreDeletion.apply(storesMap, method, dumpEnabled, dumpString.append(_))
  }

  def run(tree: JVMProgram) = {
    dumpString = new StringBuilder("")

    tree.classes.foreach((javaClass) =>
        javaClass.methods.map(methodDCE(_)))
    tree
  }
}

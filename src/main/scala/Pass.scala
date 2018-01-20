package toplev

import utils.FileUtils
import exceptions.UnreachableException

/*
 * This is a class that provides a base class for
 * all passes.
 *
 * Passes from (say) AST  -> AST should have
 * InputType = OutputType = AST.
 */

object Pass {
  var passNumber = 0;
}

abstract class Pass[InputType, OutputType <: GenericPrintable]
    (val passName: String) {
  // This can be set to true if specified in the arguments to execute.
  var dumpEnabled = false

  protected def run(tree: InputType): OutputType
  def execute(tree: InputType, shouldDump: Boolean): OutputType = {
    dumpEnabled = shouldDump
    
    val startTime = System.currentTimeMillis()
    val result = run(tree)
    val totalTime = System.currentTimeMillis() - startTime

    if (Shared.compileStats) {
      println("Time for pass '" + passName + "." + Pass.passNumber + "' = " +
              totalTime + "ms")
    }

    // Keep dump enabled as false between passes.
    dumpEnabled = false

    if (shouldDump) {
      dump(result)
    }

    // This pass has finished running, so increment the pass number.
    Pass.passNumber += 1
    return result
  }

  def treeToString(tree: OutputType): String =
    tree.prettyPrint

  def dump(tree: OutputType) = {
    FileUtils.writeStringToFile(Shared.filename + "." +
      Pass.passNumber.toString + "." + passName, treeToString(tree))
  }

  def unreachable =
    throw new UnreachableException()
}

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

abstract class Pass[InputType, OutputType](val passName: String) {
  protected def run(tree: InputType): OutputType
  def execute(tree: InputType, shouldDump: Boolean): OutputType = {
    val result = run(tree)
    if (shouldDump) {
      dump(result)
    }

    // This pass has finished running, so increment the pass number.
    Pass.passNumber += 1
    return result
  }

  def treeToString(tree: OutputType): String

  def dump(tree: OutputType) = {
    FileUtils.writeStringToFile(Shared.filename + "." +
      Pass.passNumber.toString + "." + passName, treeToString(tree))
  }

  def unreachable =
    throw new UnreachableException()
}

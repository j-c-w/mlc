package toplev


abstract class OptionalPass[Tree <: GenericPrintable](val name: String)
    extends Pass[Tree, Tree](name) {
  def optionalExecute(shouldExecute: Boolean, tree: Tree,
                      shouldDump: Boolean) = {
    if (shouldExecute) {
      execute(tree, shouldDump)
    } else {
      Pass.passNumber += 1
      tree
    }
  }
}

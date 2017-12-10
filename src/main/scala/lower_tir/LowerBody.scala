package lower_tir

import tir._
import byteR._

/* This does the work of lowering a function expression. It does not extend
 * the built in walks so that we can get warnings about unimplemented
 * cases.  All cases should generate something (or be exlicitly marked
 * for why they do not!) so it is sensible to  do the walk from scratch.
 */
object LowerBody {
  def apply(exp: TExp, directArgumentCount: Int,
            localsCount: Int, env: TTypeEnv) = {
    // We expect to initialize all registers to null at the start of the
    // function.  The JVM does not like it when registers are left
    // potentially unused on some (possibly never touched) path.
    //
    // Store all except the number of arguments, which are untouchable.
    val localInitialization = ((directArgumentCount until localsCount) map {
      case (n: Int) => List(JVMNullPush(), JVMLocalAStore(n + 1))
    }).toList.flatten

    val otherInstrs = LowerExp(exp, env)
    // Now that we have the instructions, do an instruction walk to
    // see how deep the stack can get:
    val maxDepth = (new StackDepthWalk(otherInstrs)).findDepth
    // If we want to insert a .linenumbertable
    // directive for mapping instructions to line numbers
    // in the source (for better exceptions), then this is
    // the place to do that.
    (maxDepth, localInitialization ::: otherInstrs)
  }
}

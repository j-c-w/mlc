package change_names

import toplev.Pass
import tir._
import exceptions.ICE

import scala.collection.mutable.{Map,HashMap}

/* This pass is currently disabled. It is a useful pass if there
 * is some language that needs to uniqueify identifiers that don't
 * have uniqueness problems within envs.
 *
 * The problem with using this for SML is (a) that we need to know
 * the types of all items in the tree, which we  can only get
 * if we uniqueify before typechecking and (b) this algorithm
 * actually fails if there is only one entry for a sequence of vals
 * of the same name:
 *  val x = 1
 *  val x = "HI"
 *
 * Has the type env: x: string.
 * So on the first rename, we get:
 *
 *  val $a#x = 1
 *  val x = "HI"
 *
 *  $x#a : string
 *
 * And clearly this is broken. There is no quick fix to this,  but
 * other than this bug, this pass is functional, so it is left here.
 */

class ChangeNamesWalk extends TPass {
  /* Note that although it is tempting to try and merge indistinguishable
   * nodes here (e.g. make two references to the same identifier the
   * same node), it is a potentially confusing idea for later manipulation
   * stages (we very much do NOT want a transform changing random
   * bits of the tree
   */
  val nameMap: Map[String, String] = new HashMap[String, String]()

  // This is a rather ugly hack. This is the only STD library function
  // that we treat as a function without a long name. It must map
  // to itself unless it is overriden.
  nameMap("print") = "print"

  override def apply(env: TTypeEnv, ident: TIdent) = ident match {
    case ident @ TIdentVar(oldName) =>
      if (nameMap.contains(oldName)) {
        ident.name = nameMap(oldName)
      } else {
        throw new ICE("""Expected to find %s in map
          |%s but did not.""".stripMargin.format(oldName, printNameMap()))
      }

      // The return value doesn't actually matter in this case
      true
    case _ => true
  }

  override def apply(env: TTypeEnv, fun: TFun) = {
    // Generate a new name for the function:
    val newName = FunctionNameGenerator.newFunctionName(fun.name.name)
    val newIdent = TIdentVar(newName)

    // Update the type environment:
    env.swapNames(fun.name, newIdent)

    // Add to the local map
    nameMap(fun.name.name) = newName

    // And update the function name
    fun.name = newIdent

    // We manually walk the function since
    // we replaced the name here.
    fun.patterns.foreach(_.walk(env, this))
    false
  }

  override def apply(env: TTypeEnv, pat: TPat) = pat match {
    case patVar @ TPatVariable(oldName) => {
      val newName = FunctionNameGenerator.newIdentName(oldName.name)
      val newIdent = TIdentVar(newName)
      nameMap(oldName.name) = newName

      env.swapNames(oldName, newIdent)
      patVar.variable = newIdent
      // This must be false or we will walk the new val-dec again...
      false
    }
    case _ => true
  }

  override def apply(env: TTypeEnv, dec: TVal) = {
    // We walk the dec with a specially delcared pass
    // just for adding declarations. This gives additional
    // safety (as it allows us to throw if the expression
    // contains an as of yet unidentified value)
    // Do the RHS first, as an expression like this:
    //    val x = 10
    //    val x = x + 1
    //     - it = 11
    dec.exp.walk(env, this)

    // Do the LHS to set up the change map last:
    dec.ident.walk(env, ValDecWalk)

    // And stop this val being walked automatically
    false
  }

  /* This object is a private object designed for walking the
   * LHS declarations of values only. */
  object ValDecWalk extends TPass {
    override def apply(env: TTypeEnv, ident: TIdent) = ident match {
      case oldIdent @ TIdentVar(oldName) => {
        val newName = FunctionNameGenerator.newIdentName(oldName)
        val newIdent = TIdentVar(newName)
        nameMap(oldName) = newName

        // Also update the type environment with the new name
        env.swapNames(oldIdent, newIdent)

        oldIdent.name = newName
        false
      }
      case _ => true
    }

    override def apply(env: TTypeEnv, p: TConst): Boolean = unreachable
    override def apply(env: TTypeEnv, p: TExp): Boolean = unreachable
    override def apply(env: TTypeEnv, p: TFun): Boolean = unreachable
    override def apply(env: TTypeEnv, p: TPat): Boolean = unreachable
    override def apply(env: TTypeEnv, p: TType): Boolean = unreachable
    override def apply(env: TTypeEnv, p: TVal): Boolean = unreachable
    override def apply(p: TProgram): Boolean = unreachable
    override def apply(p: TProgramOrdered): Boolean = unreachable

    def unreachable = throw new ICE("""Error: ValDecWalk object
      may only be used to walk L-Values.""")
  }

  private def printNameMap(): String = nameMap.mkString("\n")
}


package copy_prop

import def_finder.DefFinder
import exceptions.ICE
import scala.collection.mutable.HashSet
import tir._
import tir_utils.AssignmentDeletionWalk
import toplev.OptionalPass

/* See README for more details.  */
object TCopyPropagation extends OptionalPass[TJavaProgram]("copy_prop") {
  var copyPropCount = 0

  def copyProp(function: TJavaFun) = function match {
    case TJavaFun(name, curriedArgs, funLet @ TExpFunLet(valdecs, exp),
                  env) => {
      val copyPropagated = new HashSet[TNamedIdent]()
      valdecs.foreach {
        case ident =>
          DefFinder.getSingleAssign(env, funLet, ident) match {
            case Some((typeEnv, dec)) => dec match {
              case TExpAssign(name: TNamedIdent, exp) =>
                copyPropExp(exp) match {
                  case Some(otherVariable) => {
                    val copyPropWalk =
                      new CopyPropagationReplacementWalk(ident,
                                                         otherVariable, funLet,
                                                         env)
                    copyPropWalk((), funLet)
                    // Remove the variable from the declared vvariables list.
                    valdecs.remove(ident)
                    // Now, add the variable assigned to to the variables to
                    // delete.
                    copyPropagated += ident
                    copyPropCount += 1
                    dumpString("Variable " + ident + " removed and " +
                               "replaced with " + otherVariable + ".\n")
                  }
                  case None =>
                    dumpString("Variable " + ident + " not defined" +
                               " in a copy propagatable manner.\n")
                }
              case TExpAssign(nonNamedIdent, exp) =>
                dumpString("Variable appeas to not be a named ident.  " +
                           "Cannot copy propagate " + nonNamedIdent + ".\n")
                        
              case other => throw new ICE("Unexpected function in funlet")
            }
            case None =>
              dumpString("Variable " + ident +
                         " not copy propagated due to multiple defs.\n")
          }
      }

      // Finally, we go through all the variables we have copy propagated
      // and delete their definitions.
      AssignmentDeletionWalk(copyPropagated, funLet)
    }
  }

  /* Check if the expression is copy propagatable.
   *
   * If so, return the new ident.  Otherwise, return None.
   */
  def copyPropExp(exp: TExp): Option[TExp] = exp match {
    case TExpIdent(ident: TNamedIdent) => Some(exp)
    case TExpConst(_: TConstInt)
       | TExpConst(_: TConstChar)
       | TExpConst(_: TConstFloat) 
       | TExpConst(_: TConstBool) => Some(exp)
    case _ => None
  }

  def run(tree: TJavaProgram) = {
    copyPropCount = 0
    copyProp(tree.main)
    tree.functions.foreach(copyProp(_))
    dumpString("Number of copy propagations is " + copyPropCount + "\n")
    tree
  }
}

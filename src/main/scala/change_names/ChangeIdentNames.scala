package change_names

import scala.collection.mutable.{HashMap,Map}
import tir._
import tpass.TTypeEnvUnitPass
import typecheck.VariableGenerator

/* This is a class that provides utilities for changing names.
 *
 * It also keeps the type environment updated.  However, the type
 * environment is only updated in the scope that is /seen/ by
 * this pass.  That is to say that we only update the type environment
 * when we touch the variable.
 *
 * The old variable is not delted from the type environment because that is
 * actually a hard problem.  (It requires another walk of the tree to
 * ensure that the variable is not used).  If speed becomes a problem
 * due to large type environment, then a specialized pass will have
 * to be implemented.
 */

object ChangeIdentNames extends TTypeEnvUnitPass {
  def newNamesFor(namesToReplace: Map[TNamedIdent, (TNamedIdent, TType)],
                  expression: TExp, env: TTypeEnv): Unit = {
    val walk  = new ChangeIdentNamesWalk(namesToReplace)

    walk.apply(env, expression)
  }

  /* This function implements the above transformation.
   * Namely, it replaces all the names in 'namesToReplace' that are found
   * in 'expression with new variables.  It keeps the type environments
   * updated, copying the types from 'types' into 'expressionEnv'
   *
   * 'expressionEnv's scope is kept as small as possible.
   */
  def newNamesFor(toReplace: List[(TNamedIdent, TType)],
                  expression: TExp, expressionEnv: TTypeEnv): Unit = {
    val map: Map[TNamedIdent, (TNamedIdent, TType)] =
      new HashMap[TNamedIdent, (TNamedIdent, TType)]()

    for ((name, typ) <- toReplace) {
      map(name) = (VariableGenerator.newTVariable(), typ)
    }

    newNamesFor(map, expression, expressionEnv)
  }
}

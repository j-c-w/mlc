package lower_program

import scala.collection.mutable.{HashSet,Set}
import tir._
import tpass.TTypeEnvPass

/* This pass looks through an expression,
 * returning all variables that are declared in Let-bindings
 * and case statements.
 *
 * It also updates the top level envrionment that is passed
 * with the appropriate type information for these declared
 * identifiers.
 */

class GatherLetIdentifiersWalk(parentEnv: TTypeEnv)
    extends TTypeEnvPass[Set[TIdentVar]] {
  def combine(s1: Set[TIdentVar], s2: Set[TIdentVar]) =
    s1.union(s2)

  def default = new HashSet[TIdentVar]()

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case (tval: TVal) => {
      val returnSet = getDecsFrom(tval.ident)

      for (atomicIdent <- returnSet.toList) {
        parentEnv.add(atomicIdent, env.getOrFail(atomicIdent), false)
      }

      returnSet
    }
    case other => super.apply(env, dec)
  }

  /* Given some identifier that may be an identifier
   * with nested contents, return the atomic TIdentVars
   * from within it.
   */
  def getDecsFrom(ident: TIdent): Set[TIdentVar] = ident match {
    case TIdentTuple(subIdents) =>
      combineList(subIdents.map(getDecsFrom(_)))
    case identVar @ TIdentVar(variable, identClass) => {
      val set = new HashSet[TIdentVar]()
      set += identVar
      set
    }
    case other => default
  }
}

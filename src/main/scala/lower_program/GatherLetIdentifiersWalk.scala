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
    extends TTypeEnvPass[Set[TNamedIdent]] {
  def combine(s1: Set[TNamedIdent], s2: Set[TNamedIdent]) =
    s1.union(s2)

  def default = new HashSet[TNamedIdent]()

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case (tval: TVal) => {
      val returnSet = getDecsFrom(tval.ident)

      for (atomicIdent <- returnSet.toList) {
        parentEnv.add(atomicIdent, env.getOrFail(atomicIdent), false)
      }

      val internalDecs = super.apply(env, tval)

      combine(returnSet, internalDecs)
    }
    case other => super.apply(env, dec)
  }

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    // Variables may also be declared in assign nodes.
    case TExpAssign(ident, assignExp) => {
      val thisIdent = new HashSet[TNamedIdent]
      thisIdent += ident
      combine(thisIdent, super.apply(env, exp))
    }
    case other => super.apply(env, exp)
  }

  /* Given some identifier that may be an identifier
   * with nested contents, return the atomic TIdentVars
   * from within it.
   */
  def getDecsFrom(ident: TIdent): Set[TNamedIdent] = ident match {
    case TIdentTuple(subIdents) =>
      combineList(subIdents.map(getDecsFrom(_)))
    case identVar @ TIdentVar(variable, identClass) => {
      val set = new HashSet[TNamedIdent]()
      set += identVar
      set
    }
    case namedIdent @ TMutableIdent(name, identClass) => {
      val set = new HashSet[TNamedIdent]()
      set += namedIdent
      set
    }
    case other => default
  }
}

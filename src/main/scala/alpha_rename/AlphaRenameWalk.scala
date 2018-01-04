package alpha_rename

import exceptions.ICE
import scala.collection.mutable.HashMap
import tir._
import tpass.TTypeEnvUnitPass
import typecheck.VariableGenerator

/* Note that this walk does not mandate reaching the definition before the
 * use of a variable.  This is achieved with two walks of the tree.
 *
 * The assigns values to the identifier names, and the second applies them.
 */

class AlphaRenameWalk extends TTypeEnvUnitPass {
  val renameMap = new HashMap[TNamedIdent, TNamedIdent]()

  override def apply(u: TTypeEnv, exp: TExp) = exp match {
    case TExpMatchRow(pat, exp, env) => {
      pat.foreach(apply(env, _))
      apply(env, exp)
    }
    case other => super.apply(u, other)
  }

  override def apply(env: TTypeEnv, pat: TPat): Unit = pat match {
    case TPatVariable(identVar) =>
      mapIdents(env, identVar)
    case TPatIdentifier(ident) =>
      mapIdents(env, ident)
    case TPatSeq(elems) =>
      elems.map(apply(env, _))
    case TListPat(elems) =>
      elems.map(apply(env, _))
    case TPatConst(_) => // Do nothing in this case.
    case TPatWildcard() => // Do nothing in this case. 
    case TPatCons(hd, tl) => {
      apply(env, hd)
      apply(env, tl)
    }
  }

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case TFun(name, pats) => {
      mapIdents(env, name)
      pats.foreach(super.apply(env, _))
    }
    case TVal(idents, exp) => {
      mapIdents(env, idents)
      super.apply(env, exp)
    }
    case TJavaFun(name, curriedArgs, exp, env) => {
      mapIdents(env, name)
      curriedArgs.map(super.apply(env, _))
      super.apply(env, exp)
    }
  }

  /* This function deals with special cases, like whether the ident
   * is a tuple or not.  */
  private def mapIdents(env: TTypeEnv, ident: TIdent): Unit = ident match {
    case TIdentTuple(elems) =>
      elems.map(mapIdents(env, _))
    case namedIdent @ TIdentVar(name, identClass) => {
      renameMap(namedIdent) = VariableGenerator.newTVariable(identClass)
      env.add(namedIdent, env.getOrFail(namedIdent), false)
    }
    case TIdentLongVar(idents, _) =>
      throw new ICE("Declaration of LongIdent being alpha renamed")
    case argument: TArgumentNode =>
      // There is nother particularly difficult about supporting this case.
      // Careful thought needs to be done about what to alpha rename the
      // variable to, so it has been omitted for now.
      throw new ICE("AlphaRenaming after LowerProgram not currently supported")
    case numberedIdent: TNumberedIdentVar =>
      // Again, careful thought must  be given to what to rename a number
      // to.
      throw new ICE("AlphaRenaming after NumberIdents not currently supported")
    case TTopLevelIdent(_, _) =>
      throw new ICE("AlphaRenaming at top level is not currently supported")
    case other =>
      // Do nothing.
  }
}

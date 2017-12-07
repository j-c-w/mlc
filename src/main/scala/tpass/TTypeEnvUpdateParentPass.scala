package tpass

import tir._

class TTypeEnvUpdateParentPass extends TParentSetPass[TTypeEnv] {
  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case letExpr @ TExpLetIn(decs, exp, letEnv) => {
      onTouchEnv(letEnv)

      val decsResults = decs.map(apply(letEnv, _))
      val expResult = apply(letEnv, exp)

      letExpr.decs = getNew(decs, decsResults)

      expResult.map(res => letExpr.exp = res)

      None
    }
    case matchRow @ TExpMatchRow(pats, exp, rowEnv) => {
      onTouchEnv(rowEnv)

      val expResult = apply(rowEnv, exp)
      val patResults = pats.map(apply(rowEnv, _))

      matchRow.pat = getNew(pats, patResults)

      expResult.map(res => matchRow.exp = res)

      None
    }
    case other => super.apply(env, exp)
  }

  override def apply(passedEnv: TTypeEnv, dec: TDec) = dec match {
    case fundec @ TJavaFun(name, curriedArgs, exp, env) => {
      onTouchEnv(env)

      val nameResult = apply(env, name)
      val expResult = apply(env, exp)
      val curriedArgsResults = curriedArgs.map(apply(env, _))

      fundec.name =
        getNew(name, nameResult.map(_.asInstanceOf[TTopLevelIdent]))
      fundec.exp = getNew(exp, expResult.map(_.asInstanceOf[TExpFunLet]))
      fundec.curriedArgs =
        getNew(curriedArgs,
               curriedArgsResults,
               (x: TIdent) => (x.asInstanceOf[TInternalIdentVar]))

      None
    }
    case other => super.apply(passedEnv, other)
  }

  /* This is a function that can be used to update type environments
   * the first time they are touched.  */
  def onTouchEnv(env: TTypeEnv): Unit = {}
}

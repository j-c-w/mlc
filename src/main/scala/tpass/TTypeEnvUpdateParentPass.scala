package tpass

import tir._

class TTypeEnvUpdateParentPass extends TParentSetPass[TTypeEnv] {
  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case letExpr @ TExpLetIn(decs, exp, letEnv) => {
      val decsResults = decs.map(apply(letEnv, _))
      val expResult = apply(letEnv, exp)

      letExpr.decs = getNew(decs, decsResults)

      expResult.map(res => letExpr.exp = res)

      None
    }
    case matchRow @ TExpMatchRow(pats, exp, rowEnv) => {
      val expResult = apply(rowEnv, exp)
      val patResults = pats.map(apply(rowEnv, _))

      matchRow.pat = getNew(pats, patResults)

      expResult.map(res => matchRow.exp = res)

      None
    }
    case matchRow @ TExpFunLetMatchRow(pats, exp, rowEnv) => {
      val expResult = apply(rowEnv, exp)
      val patResults = pats.map(apply(rowEnv, _))

      matchRow.pat = getNew(pats, patResults)

      expResult.map(res => matchRow.exp = res.asInstanceOf[TExpFunLet])
      None
    }
    case other => super.apply(env, exp)
  }

  override def apply(passedEnv: TTypeEnv, dec: TDec) = dec match {
    case fundec @ TSimpleFun(name, exp, env) => {
      val nameResult = apply(env, name)
      val expResult = apply(env, exp)

      fundec.name = getNew(name, nameResult.map(_.asInstanceOf[TIdentVar]))
      fundec.exp = getNew(exp, expResult)

      None
    }
    case other => super.apply(passedEnv, other)
  }
}

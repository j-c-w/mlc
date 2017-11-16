package tpass

import tir._

class TTypeEnvUpdateParentPass extends TParentSetPass[TTypeEnv] {
  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case letExpr @ TExpLetIn(decs, exp, letEnv) => {
      val decsResults = decs.map(apply(letEnv, _))
      val expResult = apply(letEnv, exp)

      letExpr.decs = (decs zip decsResults) map {
        case (old, Some(newDec)) => newDec
        case (old, None) => old
      }

      expResult.map(res => letExpr.exp = res)

      None
    }
    case matchRow @ TExpMatchRow(pats, exp, rowEnv) => {
      val expResult = apply(rowEnv, exp)
      val patResults = pats.map(apply(rowEnv, _))

      matchRow.pat = (pats zip patResults) map {
        case (old, Some(newElem)) => newElem
        case (old, None) => old
      }

      expResult.map(res => matchRow.exp = res)

      None
    }
    case letExpr @ TExpFunLet(decs, exp, letEnv) => {
      val decsResults = decs.map(apply(letEnv, _))
      val expResult = apply(letEnv, exp)

      letExpr.valdecs = (decs zip decsResults) map {
        case (old, Some(newDec)) => newDec.asInstanceOf[TIdentVar]
        case (old, None) => old
      }

      expResult.map(res => letExpr.exp = res)
      None
    }
    case matchRow @ TExpFunLetMatchRow(pats, exp, rowEnv) => {
      val expResult = apply(rowEnv, exp)
      val patResults = pats.map(apply(rowEnv, _))

      matchRow.pat = (pats zip patResults) map {
        case (old, Some(newElem)) => newElem
        case (old, None) => old
      }

      expResult.map(res => matchRow.exp = res.asInstanceOf[TExpFunLet])
      None
    }
    case other => super.apply(env, exp)
  }
}

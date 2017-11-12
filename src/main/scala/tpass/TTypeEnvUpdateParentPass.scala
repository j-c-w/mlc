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

      expResult match {
        case None =>
        case Some(expResult) => letExpr.exp = expResult
      }

      None
    }
    case other => super.apply(env, exp)
  }

  override def applyMatchRow(env: TTypeEnv, matchRow: TExpMatchRow) =
    matchRow match {
      case matchRow @ TExpMatchRow(pats, exp, rowEnv) => {
        val expResult = apply(rowEnv, exp)
        val patResults = pats.map(apply(rowEnv, _))

        matchRow.pat = (pats zip patResults) map {
          case (old, Some(newElem)) => newElem
          case (old, None) => old
        }

        expResult match {
          case None =>
          case Some(expResult) => matchRow.exp = expResult
        }

        None
      }
    }
}

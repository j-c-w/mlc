package lambda_lift

import exceptions.ICE
import tir._
import scala.collection.mutable.{HashSet,Set}
import tpass.TTypeEnvUnitPass

object FreeValsWalk {
  def apply(parentEnv: TTypeEnv,
            patterns: List[TExpMatchRow]): List[(TExpIdent, TType)] = {
    var resultSet = new HashSet[(TExpIdent, TType)]()

    patterns.foreach((pat) => {
      val walk = new FreeValsWalk(pat.env)
      walk(parentEnv, pat)
      resultSet = resultSet.union(walk.freeValsSet)
    })

    resultSet.toList
  }
}

class FreeValsWalk(val functionEnv: TTypeEnv) extends TTypeEnvUnitPass {
  var freeValsSet: Set[(TExpIdent, TType)] =
    HashSet[(TExpIdent, TType)]()

  override def apply(env: TTypeEnv, dec: TDec): Unit = dec match {
    case valdec @ TVal(lhs, rhs) =>
      // This valdec is declared on some internal LetIn.
      // Therefore, we do not want to procees the LHS, and
      // we only consider the RHS.
      apply(env, rhs)
    case fndec @ TFun(name, patterns) =>
      throw new ICE("Expect inner TFun's to have been removed")
  }

  override def apply(env: TTypeEnv, exp: TExp): Unit = exp match {
    case TExpIdent(identVar @ TIdentVar(name)) => {
      // We check whether the identifier was declared between this
      // environment and the top function environment. 
      if (!env.hasTypeBetweenInclusive(functionEnv, identVar)) {
        freeValsSet +=
          ((TExpIdent(identVar), env.getNoSubstituteOrFail(identVar)))
      }
    }
    case TExpMatchRow(pattern, expr, matchRowEnv) =>
      apply(matchRowEnv, expr)
    case other => super.apply(env, exp)
  }

  override def apply(env: TTypeEnv, pat: TPat) =
    // This is thrown as walking the pattern (would probably do no harm)
    // but would be a waste of time.
    throw new ICE("Error: Cannot FreeValsWalk a pattern")
}

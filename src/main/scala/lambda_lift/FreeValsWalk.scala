package lambda_lift

import def_finder.DefFinder
import exceptions.ICE
import scala.collection.mutable.{HashSet,Set}
import tir._
import tpass.TTypeEnvUnitPass

object FreeValsWalk {
  def apply(program: TProgram, functionName: Option[TNamedIdent],
            parentEnv: TTypeEnv,
            patterns: List[TExpMatchRow]): List[(TExpIdent, TType)] = {
    var resultSet = new HashSet[(TExpIdent, TType)]()

    patterns.foreach((pat) => {
      val walk = new FreeValsWalk(program, functionName, pat.env)
      walk(parentEnv, pat)
      resultSet = resultSet.union(walk.freeValsSet)
    })

    resultSet.toList
  }
}

class FreeValsWalk(val program: TProgram,
                   val functionName: Option[TNamedIdent],
                   val functionEnv: TTypeEnv) extends TTypeEnvUnitPass {
  var freeValsSet: Set[(TExpIdent, TType)] =
    HashSet[(TExpIdent, TType)]()

  // Create the visited set with an initial member being this
  // function.
  val visited: Set[TNamedIdent] = HashSet[TNamedIdent]()
  functionName.map(visited.+=(_))

  override def apply(env: TTypeEnv, dec: TDec): Unit = dec match {
    case valdec @ TVal(lhs, rhs) =>
      // This valdec is declared on some internal LetIn.
      // Therefore, we do not want to procees the LHS, and
      // we only consider the RHS.
      apply(env, rhs)
    case other => super.apply(env, other)
  }

  override def apply(env: TTypeEnv, exp: TExp): Unit = exp match {
    case TExpIdent(identVar: TNamedIdent) => {
      // We check whether the identifier was declared between this
      // environment and the top function environment. 
      if (!env.hasTypeBetweenInclusive(functionEnv, identVar)) {
        env.getOrFail(identVar) match {
          // If this is a function, then we should walk that function
          // to find it's free variables.  Mark the function in the
          // vvisited function map so that we don't visit it twice.
          case TFunctionType(_, _) => {
            if (!visited.contains(identVar)) {
              visited += identVar
              val foundDef =
                DefFinder(program.typeEnv, program, identVar)
              foundDef match {
                case Some((otherEnv, identDef)) => apply(otherEnv, identDef)
                case None => throw new ICE("""Error: Declaration
                  |of idetifier %s was not found""".stripMargin.format(
                    identVar.prettyPrint))
              }
            }
          }
          case _ =>
            freeValsSet +=
              ((TExpIdent(identVar), env.getNoSubstituteOrFail(identVar)))
        }
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

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
                   val functionEnv: TTypeEnv,
                   val visited: Set[TNamedIdent]) extends TTypeEnvUnitPass {
  def this(program: TProgram, functionName: Option[TNamedIdent],
           functionEnv: TTypeEnv) =
     this(program, functionName, functionEnv, new HashSet[TNamedIdent]())

  var freeValsSet: Set[(TExpIdent, TType)] =
    HashSet[(TExpIdent, TType)]()

  // Create the visited set with an initial member being this
  // function.
  functionName.map(visited.+=(_))

  override def apply(env: TTypeEnv, dec: TDec): Unit = dec match {
    case valdec @ TVal(lhs, rhs) =>
      // This valdec is declared on some internal LetIn.
      // Therefore, we do not want to procees the LHS, and
      // we only consider the RHS.
      apply(env, rhs)
    case fundec @ TFun(name, rhs) => {
      // The name will be lambda lifted. Only walk the patterns.
      rhs.foreach(apply(env, _))
    }
    case other => super.apply(env, other)
  }

  override def apply(env: TTypeEnv, exp: TExp): Unit = exp match {
    case TExpIdent(identVar: TIdentVar) => {
      // We check whether the identifier was declared between this
      // environment and the top function environment.
      if (!env.hasTypeBetweenInclusive(functionEnv, identVar)) {
        identVar.identClass match {
          // If this is a function, then we should walk that function
          // to find it's free variables.  Mark the function in the
          // visited function map so that we don't visit it twice.
          case TFunClass() => {
            if (!visited.contains(identVar)) {
              val foundDef =
                DefFinder(program.typeEnv, program, identVar)
              foundDef match {
                case Some((otherEnv, identDef : TFun)) => {
                  val recursiveWalk =
                    new FreeValsWalk(program, Some(identVar),
                                     // We want to pass the same
                                     // environment here. We are interested
                                     // in the variables that are not in
                                     // the local environment after all.
                                     functionEnv, visited)

                  recursiveWalk(otherEnv, identDef)

                  recursiveWalk.freeValsSet.foreach {
                    // Only add the ident to the free vals set if it would be
                    // free in this function.
                    case ((TExpIdent(ident), typ)) =>
                      if (!env.hasTypeBetweenInclusive(functionEnv, ident)) {
                        freeValsSet += ((TExpIdent(ident), typ))
                      }
                  }
                }
                case Some((_, valdec : TVal)) =>
                  // In this case, add to the variable to the set.
                  freeValsSet +=
                    ((TExpIdent(identVar),
                      env.getNoSubstituteOrFail(identVar)))
                case Some((_, other)) =>
                  throw new ICE("Unexpected declaration type %s".format(
                    other.prettyPrint))
                case None =>
                  freeValsSet +=
                    ((TExpIdent(identVar),
                      env.getNoSubstituteOrFail(identVar)))
                  // Add the variable to the free vals set
                  // on its own.  We used to throw here,
                  // but that is incorrect, as a function passed
                  // as an argument has no declaration.
                  //
                  // If there are a lot of silenced errors due to this, a
                  // check of where the unfound definition was declared
                  // is a sensible idea.
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

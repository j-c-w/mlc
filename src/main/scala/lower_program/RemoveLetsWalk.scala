package lower_program

import exceptions.ICE
import generators.VariableGenerator
import scala.collection.mutable.{HashSet,Set}
import tir._
import tpass.TParentSetPass

/* This pass should be run AFTER the GatherLetDeclarations pass
 * has run.
 *
 * This class has the task of removing all let constructs from
 * an expression.
 *
 * The expression is modified in place to remove these let constructs.
 *
 * The val declarations are replaced with assignments.
 *
 * This pass *also* creates appropriate assignment expressions
 * for these values.
 */

class RemoveLetsWalk(val replacementEnv: TTypeEnv)
    extends TParentSetPass[Unit] {
  val accumulatedIdents: Set[TNamedIdent] = new HashSet[TNamedIdent]()

  override def apply(u: Unit, exp: TExp) = exp match {
    // IMPORTANT: Do not use the Envrionment, as it may be BS
    // by the time it gets here.  The case statement and the
    // handle statement cases makes recursive calls with a BS envs.
    case let @ TExpLetIn(decs, exp, _) => {
      // First do a walk of each of the decs and the exp
      // to make sure that inner let declarations and
      // pattern matches have been suitably removed.
      val newDecs = getNew(decs, decs.map(apply(u, _)))
      val newExp = getNew(exp, apply(u, exp))

      val (assignmentExpressions, newIdents) =
        newDecs.map(
          AssignmentGeneration.convertToAssignNodeDec(_, replacementEnv)).unzip

      // Add the identifiers to the set to return at the end.
      accumulatedIdents ++= newIdents.flatten

      newExp match {
        case TExpSeq(subSeq) =>
          Some(TExpSeq(assignmentExpressions ::: subSeq).flatten)
        case _ =>
          if (assignmentExpressions.length > 0)
            Some(new TExpSeq(assignmentExpressions :+ newExp).flatten)
          else
            Some(newExp)
      }
    }
    case caseExp @ TExpCase(exp, patterns, typeID) => {
      val newExp = apply(u, exp)
      val expressionIdentifier = VariableGenerator.newTVariable(TValClass())

      accumulatedIdents += expressionIdentifier

      // The intuitive way to represent this is with a TLet expression.
      // We do just that, and walk to to convert it to the correct
      // sequence of expressions.
      // However, the new valdec does need to be added to the top
      // level let env.
      val valType = replacementEnv.getOrFail(typeID) match {
        case TFunctionType(from, _) => from
        case other => throw new ICE("""Error: Expected case statement to have
          |a function type. Instead it had type %s""".stripMargin.format(
            other.prettyPrint))
      }
      replacementEnv.add(expressionIdentifier, valType, false)

      val patternExpression =
        patterns.foldRight(TExpRaise(TExpIdent(TIdentMatchError())): TExp) {
          case (TExpMatchRow(pats, matchExp, env), elseCond) => {
            assert(pats.length == 1)

            val (condition, newIdents) =
              AssignmentGeneration.generateAssignsFromPattern(pats,
                List(expressionIdentifier), replacementEnv)

            // Add the identifiers to the set to return at the end.
            accumulatedIdents ++= newIdents.flatten

            TExpIf(condition, matchExp, elseCond)
          }
        }

      val letDec =
        TExpLetIn(
          List(TVal(expressionIdentifier, getNew(exp, newExp))),
          patternExpression,
          new TTypeEnv())

      apply(u, letDec)
    }
    case TExpHandle(exp, cases, applicationType) => {
      // Wrap this as a try statement in a normal TExpCase.  The input is
      // provided as an unwrapped exception type that can be pattern matched
      // on like a datatype.  If that fails, we re-throw the exception.
      val newApplicationType =
        replacementEnv.getOrFail(applicationType) match {
          case TFunctionType(from, to) => TFunctionType(TExceptionType(), to)
          case _ => throw new ICE("Handle statement without function type")
        }
      val newApplicationID = VariableGenerator.newTInternalVariable()
      val tryCatchID = VariableGenerator.newTInternalVariable()
      val exceptionName = VariableGenerator.newTVariable(TValClass())

      replacementEnv.add(exceptionName, TExceptionType(), false)
      replacementEnv.addTopLevel(newApplicationID, newApplicationType, false)
      replacementEnv.addTopLevel(tryCatchID, newApplicationType, false)

      accumulatedIdents += exceptionName

      val catchCases =
        cases :+
        // Add the extra case to always match and re-throw the exception
        // if no match was already found.
        TExpMatchRow(List(TPatWildcard()),
                     TExpRaise(TExpIdent(exceptionName)),
                     // We may create an empty type env because there is
                     // nothing to be typed within the environment.
                     new TTypeEnv())

      val reducedCases =
        TExpTry(exp, exceptionName,
                TExpSeq(List(TExpAssign(exceptionName,
                                        TExpIdent(TCaughtExceptionIdent())),
                             TExpCase(TExpIdent(exceptionName),
                                      catchCases, newApplicationID))),
                tryCatchID)

      val fixedUp = apply(u, reducedCases)
      assert(fixedUp == None)
      Some(reducedCases)
    }
    case seq @ TExpSeq(elems) => {
      // Since we are generating new TExpSeqs, we have to flatten.
      super.apply(u, seq) match {
        case None => Some(seq.flatten)
        case Some(newSeq @ TExpSeq(newElems)) => Some(newSeq.flatten)
        case Some(other) => Some(other)
      }
    }
    // This case is inserted for safety.  We do not expect it to be
    // hit, and if it is, it is likely in error.  This pass is expected
    // to insert computing expressions for pattern elements
    // into the beginning of the corresponding expression.
    case TExpMatchRow(_, _, _) => throw new ICE("""
      |Error: RemoveLetsWalk expects to insert computing expressions
      |into match rows which should be done in the parents of the let
      |rows to distinguish between node types that should be treated
      |differentely""".stripMargin)
    case other =>
      super.apply(u, exp)
  }
}

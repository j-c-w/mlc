package lower_program

import exceptions.ICE
import scala.collection.mutable.{HashSet,Set}
import tir._
import tpass.TParentSetPass
import typecheck.VariableGenerator

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

class RemoveLetsWalk extends TParentSetPass[Unit] {
  val accumulatedIdents: Set[TIdentVar] = new HashSet[TIdentVar]()

  override def apply(u: Unit, exp: TExp) = exp match {
    case let @ TExpLetIn(decs, exp, env) => {
      // First do a walk of each of the decs and the exp
      // to make sure that inner let declarations and
      // pattern matches have been suitably removed.
      val newDecs = getNew(decs, decs.map(apply(u, _)))
      val newExp = getNew(exp, apply(u, exp))

      val (assignmentExpressions, newIdents) =
        newDecs.map(AssignmentGeneration.convertToAssignNodeDec(_)).unzip

      // Add the identifiers to the set to return at the end.
      accumulatedIdents ++= newIdents.flatten

      newExp match {
        case TExpSeq(subSeq) =>
          Some(TExpSeq(assignmentExpressions.flatten ::: subSeq))
        case _ =>
          Some(new TExpSeq(assignmentExpressions.flatten :+ newExp))
      }
    }
    case caseExp @ TExpCase(exp: TExp, patterns: List[TExpMatchRow]) => {
      val newExp = apply(u, exp)

      patterns.foreach {
        case row @ TExpMatchRow(pats, matchExp, env) => {
          val (assignments, newIdents) =
            AssignmentGeneration.generateAssignsFromPattern(pats).unzip

          // Add the identifiers to the set to return at the end.
          accumulatedIdents ++= newIdents.flatten

          row.exp = TExpSeq(assignments.flatten :+ matchExp)
        }
      }

      caseExp.exp = getNew(exp, newExp)
      None
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
    case other => super.apply(u, exp)
  }
}

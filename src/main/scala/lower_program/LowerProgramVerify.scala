package lower_program

import exceptions.ICE
import environment_soundness.EnvironmentSoundnessWalk
import scala.collection.mutable.{HashSet,Set}
import tir._
import toplev.OptionalPass
import tpass._

/* This is a pass that can be used in testing to ensure
 * that the lower_program has done what it said it would.
 *
 * Namely, it checks:
 *
 *  (1) That there are no let bindings left in the program.
 *  (2) That every variable is 'Assign'ed to before use
 *  (3) That every variable occurs in the bindign FunLet.
 */

object LowerProgramVerify
    extends OptionalPass[TJavaProgram]("lower_program_verify") {
  def run(tree: TJavaProgram) = {
    // Do the let binding verification:
    NoLetsWalk.apply((), tree)

    // Now check that every variable is in a FunLet.
    (new FunLetsIntegrityWalk()).apply(tree.typeEnv, tree)

    // Finally, check that every variable is assigned to before
    // it is used.
    (new AssignIntegrityWalk()).apply((), tree)

    EnvironmentSoundnessWalk(tree.typeEnv, tree)
    tree
  }
}

object NoLetsWalk extends TUnitPass {
  override def apply(u: Unit, expr: TExp) = expr match {
    case TExpLetIn(_, _, _) => throw new ICE("""NoLetsWalk failed. Expected
      |all lets to be removed after the LowerProgram pass, but this
      |has not happened. Let was:
      |%s""".stripMargin.format(expr.prettyPrint))
    // Also check that there are no nested sequences:
    case seq @ TExpSeq(elems) => {
      if (elems.length <= 1) {
        throw new ICE("ExpSeq with only one element is not allowed")
      }

      elems.foreach {
        case TExpSeq(_) =>
          throw new ICE("Nested ExpSeq found which is not valid")
        case _ =>
      }

      // Then walk the seq normally.
      super.apply(u, seq)
    }
    case _ => super.apply(u, expr)
  }
}

class FunLetsIntegrityWalk extends TTypeEnvUnitPass {
  val variablesInLets: Set[TNamedIdent] = new HashSet[TNamedIdent]()

  override def apply(env: TTypeEnv, program: TJavaProgram) = {
    // Add all the functions first.
    (program.main :: program.functions).foreach {
      case TJavaFun(ident, curriedArgs, exprs, env) =>
        variablesInLets += ident
    }

    apply(program.typeEnv, program.main)
    program.functions.foreach(apply(program.typeEnv, _))
  }

  override def apply(typeEnv: TTypeEnv, ident: TIdent) = ident match {
    case namedIdent: TNamedIdent => {
      namedIdent match {
        case identVar @ TIdentVar(name, identClass) => {
          if (!variablesInLets.contains(identVar)) {
            throw new ICE("""Error, variable %s was not declared
              |in any let binding""".stripMargin.format(identVar))
          }
        }
        case _ =>
      }

      // Also check that the type of the identifier within the environment
      // is OK.  We may avoid checking whether the type is in the environment
      // since that is the task of the environment_soundness walk.
      //
      // LongIdentifiers might not be in the typeEnvironment at this
      // point.
      if (typeEnv.hasType(namedIdent))
        typeEnv.getOrFail(namedIdent) match {
          case TTupleType(subTypes) =>
            if (subTypes.length == 0 || subTypes.length == 1) {
              throw new ICE("""Found a variable with tuple type of size
              |%s.  This should not have happened.
              |""".stripMargin.format(subTypes.length))
            }
          case other =>
        }
    }
    case TIdentTuple(subIdents) =>
      throw new ICE("""Error: Expected all ident tuples to be removed by
        |the lower_program pass""".stripMargin)
    case other => super.apply(typeEnv, other)
  }

  override def apply(typeEnv: TTypeEnv, exp: TExp) = exp match {
    case TExpFunLet(decs, exp) => {
      variablesInLets ++= decs

      apply(typeEnv, exp)
    }
    case TExpTuple(elems) => {
      if (elems.size <= 1) {
        throw new ICE("""Error: Expected Lower Program to have removed
          |all the nearly empty tuples.  Instead, found a tuple of length
          |%s""".stripMargin.format(elems.size.toString))
      }
    }
    case TExpFunApp(exp, app, typ) => {
      // We do not walk the typ here.
      apply(typeEnv, exp)
      apply(typeEnv, app)
    }
    case TExpListHead(exp, typ) =>
      apply(typeEnv, exp)
    case TExpTupleExtract(exp, index, size, typ) =>
      apply(typeEnv, exp)
    case TExpListExtract(exp, index, typ) =>
      apply(typeEnv, exp)
    case other => super.apply(typeEnv, other)
  }
}

class AssignIntegrityWalk extends TUnitPass {
  val assignedVariables: Set[TNamedIdent] = new HashSet[TNamedIdent]()

  override def apply(u: Unit, program: TJavaProgram) = {
    // Add all the functions first.
    (program.main :: program.functions).foreach {
      case TJavaFun(ident, curriedArgs, exprs, env) =>
        assignedVariables += ident
    }

    apply(u, program.main)
    program.functions.foreach(apply(u, _))
  }

  override def apply(u: Unit, exp: TExp) = exp match {
    case TExpAssign(ident, exp) => {
      // Walk the expression
      apply(u, exp)

      // And then insert the identifier. This is done second intentionally,
      // identifiers definitely should not be used in their assignments!
      assignedVariables += ident
    }
    case TExpIdent(ident @ TIdentVar(name, identClass)) => {
      if (!assignedVariables.contains(ident)) {
        throw new ICE("""Error: Lower program seems to have allowed
          |variable %s to be used before
          |assignment""".stripMargin.format(ident.prettyPrint))
      }
    }
    case TExpFunApp(exp, app, typ) => {
      // We do not walk the typ here.
      apply(u, exp)
      apply(u, app)
    }
    case TExpIdent(_) =>
      super.apply(u, exp)
    case _ => super.apply(u, exp)
  }

  override def apply(u: Unit, ident: TIdent) = ident match {
    case TIdentTuple(_) => throw new ICE("""Error: expected all
      |ident tuples to be removed by LowerProgram pass""".stripMargin)
    case _ => super.apply(u, ident)
  }

  override def apply(u: Unit, dec: TDec) = dec match {
    case TJavaFun(name, curriedArgs, rhs, env) => {
      assignedVariables.add(name)
      apply(u, rhs)
    }
    case _ => super.apply(u, dec)
  }
}

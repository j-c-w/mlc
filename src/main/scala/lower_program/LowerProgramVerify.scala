package lower_program

import exceptions.ICE
import environment_soundness.EnvironmentSoundnessWalk
import scala.collection.mutable.{HashSet,Set}
import tir._
import toplev.OptionalPass
import tpass.{TPass,TUnitPass}

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
    (new FunLetsIntegrityWalk()).apply((), tree)

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
    case _ => super.apply(u, expr)
  }
}

class FunLetsIntegrityWalk extends TUnitPass {
  val variablesInLets: Set[TNamedIdent] = new HashSet[TNamedIdent]()


  override def apply(u: Unit, program: TJavaProgram) = {
    // Add all the functions first.
    (program.main :: program.functions).foreach {
      case TJavaFun(ident, exprs, env) => variablesInLets += ident
    }

    apply(u, program.main)
    program.functions.foreach(apply(u, _))
  }

  override def apply(u: Unit, ident: TIdent) = ident match {
    case identVar @ TIdentVar(name) =>
      if (!variablesInLets.contains(identVar)) {
        throw new ICE("""Error, variable %s was not declared
          |in any let binding""".stripMargin.format(identVar))
      }
    case other => super.apply(u, other)
  }
  
  override def apply(u: Unit, exp: TExp) = exp match {
    case TExpFunLet(decs, exp) => {
      variablesInLets ++= decs

      apply(u, exp)
    }
    case TExpFunApp(exp, app, typ) => {
      // We do not walk the typ here.
      apply(u, exp)
      apply(u, app)
    }
    case other => super.apply(u, other)
  }

  override def apply(u: Unit, dec: TDec) = dec match {
    case TJavaFun(name, rhs, env) =>
      apply(u, rhs)
    case _ => super.apply(u, dec)
  }
}

class AssignIntegrityWalk extends TUnitPass {
  val assignedVariables: Set[TNamedIdent] = new HashSet[TNamedIdent]()

  override def apply(u: Unit, program: TJavaProgram) = {
    // Add all the functions first.
    (program.main :: program.functions).foreach {
      case TJavaFun(ident, exprs, env) => assignedVariables += ident
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
    case TExpIdent(ident @ TIdentVar(name)) => {
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
    case TJavaFun(name, rhs, env) => {
      assignedVariables.add(name)
      apply(u, rhs)
    }
    case _ => super.apply(u, dec)
  }
}

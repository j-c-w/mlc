package lower_variables

import exceptions.ICE
import scala.collection.mutable.{HashMap,HashSet,Map}
import tir._
import tpass.TParentSetPass

class NumberVariablesWalk() extends TParentSetPass[Unit] {
  val variableMap: Map[TNamedIdent, TNumberedIdentVar] =
    new HashMap[TNamedIdent, TNumberedIdentVar]()

  override def apply(u: Unit, exp: TExp) = exp match {
    case funLet @ TExpFunLet(decs, exp) => {
      val numbers = (0 until funLet.valdecs.size) zip funLet.valdecs

      val newIdents = numbers.map {
        case (i, dec @ TIdentVar(name, identClass)) => {
          // Putting this in a TNumberedIdentVar implies that it is something
          // that can be stored in a register.  We should check that.
          assert(identClass.isRegisterClass)

          val newIdent = TNumberedIdentVar(name, i)
          variableMap(dec) = newIdent
          newIdent
        }
        case (i, dec @ TMutableIdent(name, identClass)) => {
          assert(identClass.isRegisterClass)

          val newIdent = TNumberedIdentVar(name, i)
          variableMap(dec) = newIdent
          newIdent
        }
        // We expect that the fun let only contains ident vars.
        case (_, other) =>
          throw new ICE("Unexpected ident type %s".format(other.prettyPrint))
      }

      // Update the valdecs:
      funLet.valdecs = new HashSet[TNamedIdent]()
      funLet.valdecs ++= newIdents

      // Walk the expression for variables.
      funLet.exp = getNew(exp, apply(u, exp))
      None
    }
    case other => super.apply(u, other)
  }

  override def apply(u: Unit, ident: TIdent) = ident match {
    case ident @ TIdentVar(name, identClass) => {
      if (variableMap.contains(ident)) {
        Some(variableMap(ident))
      } else {
        None
      }
    }
    case other => super.apply(u, other)
  }

  override def apply(u: Unit, fun: TDec) = fun match {
    case TJavaFun(name, curriedArgs, rhs, env) =>
      // We do not want to apply this to the function name.
      apply(u, rhs)
      None
    case other => throw new ICE("""Error: Found a val or a fun
      |after vals and funs have been removed""".stripMargin)
  }
}

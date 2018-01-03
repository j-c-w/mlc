package environment_soundness

import exceptions.ICE
import tir._
import tpass.TTypeEnvUnitPass
import scala.collection.mutable.HashSet

class EnvironmentSoundnessWalk(topEnv: TTypeEnv) extends TTypeEnvUnitPass {
  val seenMap = new HashSet[TInternalIdentVar]()

  override def apply(env: TTypeEnv, ident: TIdent) = ident match {
    // We should check all types of variable.
    case variable: TNamedIdent => variable match {
      case ident @ TInternalIdentVar(_) => {
        if (!topEnv.hasType(ident)) {
          throw new ICE("""Error: Internal Variable %s is not typed
            |by the top level environment. """.
            stripMargin.format(ident.prettyPrint))
        }

        // Also require that internal ident variables are only used
        // once.
        if (seenMap.contains(ident)) {
          throw new ICE("""Internal Ident Var used twice illegally.
            |Var is %s""".stripMargin.format(ident.prettyPrint))
        } else {
          seenMap += ident
        }
      }
      case  TTopLevelIdent(_, _)
          | TIdentVar(_, _)
          | TNumberedIdentVar(_, _)
          | TArgumentNode(_, _) =>
        if (!env.hasType(variable)) {
          throw new ICE("""Error: Variable %s is not typed by the envrionment""".
            format(variable.prettyPrint))
        }
      case TIdentLongVar(_, _) => // Do nothing.
    }
    case other => super.apply(env, other)
  }
}

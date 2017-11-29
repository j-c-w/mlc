package environment_soundness

import exceptions.ICE
import tir._
import tpass.TTypeEnvUnitPass

object EnvironmentSoundnessWalk extends TTypeEnvUnitPass {
  override def apply(env: TTypeEnv, ident: TIdent) = ident match {
    // We should check all types of variable.
    case variable: TNamedIdent => variable match {
      case  TTopLevelIdent(_)
          | TIdentVar(_)
          | TNumberedIdentVar(_, _)
          | TArgumentNode(_, _) =>
        if (!env.hasType(variable)) {
          throw new ICE("""Error: Variable %s is not typed by the envrionment""".
            format(variable.prettyPrint))
        }
      case TIdentLongVar(_) => // Do nothing.
    }
    case other => super.apply(env, other)
  }
}

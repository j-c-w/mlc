package lower_program
 
import exceptions.ICE
import tir._
import tpass.TTypeEnvPass
 
class MergeTypeEnvsWalk(val newEnv: TTypeEnv) extends TTypeEnvPass[Unit] {
  def combine(u: Unit, v: Unit) = u
  def default = ()
 
  override def apply(typeEnv: TTypeEnv, ident: TIdent) = ident match {
    case identVar : TNamedIdent => identVar match {
      case TIdentVar(_, _)
         | TTopLevelIdent(_, _)
         | TInternalIdentVar(_) =>
             if (!newEnv.hasType(identVar))
               typeEnv.insertInto(identVar, newEnv)
      case TIdentLongVar(_, _) => // Do nothing. This is a standard
        // library call.
      case TArgumentNode(_, _) =>
        throw new ICE("TArgument node generated before lower_program")
      case TNumberedIdentVar(_, _) =>
        throw new ICE("TNumberedIdentVar generated before the numbering pass")
    }
    case other => super.apply(typeEnv, other)
  }
}

package lower_program
 
import tir._
import tpass.TTypeEnvPass
 
class MergeTypeEnvsWalk(val newEnv: TTypeEnv) extends TTypeEnvPass[Unit] {
  def combine(u: Unit, v: Unit) = u
  def default = ()
 
  override def apply(typeEnv: TTypeEnv, ident: TIdent) = ident match {
    case identVar @ TIdentVar(name) => {
      if (!newEnv.hasType(identVar))
        typeEnv.insertInto(identVar, newEnv)
    }
    case other => super.apply(typeEnv, other)
  }
}

package type_env

import exceptions.ICE
import tir._
import tpass.TPass

/* Given some type env as argument, this pass walks through the
 * expression and inserts that environment as the new parent
 * of the FIRST environment it encounters.  This is designed
 * to insert a type environment into a chain of environments.  */

object TypeEnvUpdateParentWalk extends TPass[TTypeEnv, Unit] {
  override def combine(u: Unit, v: Unit) = u
  override def combineList(u: List[Unit]) = ()
  override def default = ()

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case let: TExpLetIn => {
      assert(let.env.parent == env.parent)
      let.env.parent = Some(env)
    }
    case matchRow: TExpMatchRow => {
      assert(matchRow.env.parent == env.parent)
      matchRow.env.parent = Some(env)
    }
    case other => super.apply(env, other)
  }

  override def apply(env: TTypeEnv, program: TJavaProgram) =
    // If this is needed, there is no particular reason not to.
    // However, it probably means you are using type environments wrong.
    throw new ICE("Cannot add a parent to the top level environment")

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case javaFun: TJavaFun => {
      assert(javaFun.env.parent == env.parent)
      javaFun.env.parent = Some(env)
    }
    case other => super.apply(env, other)
  }
}

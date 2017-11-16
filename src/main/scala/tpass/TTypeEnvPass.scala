package tpass

import tir._

/* This is a class that provides a nicely wrapped
 * walk of a tree while automatically handling
 * the edge cases of type environments.
 *
 * Inserted to avoid massed code changes after
 * a poor original design of the TPass trait.
 */

trait TTypeEnvPass extends TPass[TTypeEnv, Unit] {
  def default = ()
  def combine(x: Unit, y: Unit) = x

  override def apply(passedEnv: TTypeEnv, exp: TExp): Unit = exp match {
    case TExpLetIn(decs, exp, env) => {
      decs.foreach(apply(env, _))
      apply(env, exp)
    }
    case TExpMatchRow(pat, exp, env) => {
      pat.foreach(apply(env, _))
      apply(env, exp)
    }
    case TExpFunLet(decs, exp, env) => {
      decs.foreach(apply(env, _))
      apply(env, exp)
    }
    case TExpFunLetMatchRow(pat, exp, env) => {
      pat.foreach(apply(env, _))
      apply(env, exp)
    }
    case other => super.apply(passedEnv, exp)
  }
}

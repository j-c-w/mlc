package tpass

import tir._

/* This is a class that provides a nicely wrapped
 * walk of a tree while automatically handling
 * the edge cases of type environments.
 *
 * Inserted to avoid massed code changes after
 * a poor original design of the TPass trait.
 */

trait TTypeEnvPass[T] extends TPass[TTypeEnv, T] {
  override def apply(passedEnv: TTypeEnv, exp: TExp): T = exp match {
    case TExpLetIn(decs, exp, env) => {
      onTouchEnv(env)
      combine(combineList(decs.map(apply(env, _))), apply(env, exp))
    }
    case TExpMatchRow(pat, exp, env) => {
      onTouchEnv(env)
      combine(combineList(pat.map(apply(env, _))), apply(env, exp))
    }
    case other => super.apply(passedEnv, exp)
  }

  override def apply(passedEnv: TTypeEnv, dec: TDec): T = dec match {
    case TJavaFun(name, exp, env) => {
      onTouchEnv(env)
      combine(apply(env, name), apply(env, exp))
    }
    case other => super.apply(passedEnv, dec)
  }

  /* This is a function that can be used to update type environments
   * the first time they are touched.  */
  def onTouchEnv(env: TTypeEnv): Unit = {}
}

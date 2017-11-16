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
      combine(combineList(decs.map(apply(env, _))), apply(env, exp))
    }
    case TExpMatchRow(pat, exp, env) => {
      combine(combineList(pat.map(apply(env, _))), apply(env, exp))
    }
    case TExpFunLetMatchRow(pat, exp, env) => {
      combine(combineList(pat.map(apply(env, _))), apply(env, exp))
    }
    case other => super.apply(passedEnv, exp)
  }
}

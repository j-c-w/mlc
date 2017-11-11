package tpass

import tir._

/* This is a class that provides a nicely wrapped
 * walk of a tree while automatically handling
 * the edge cases of type environments.
 *
 * Inserted to avoid massed code changes after
 * a poor original design of the TPass trait.
 */

trait TTypeEnvPass extends TPass[TTypeEnv] {
  override def apply(passedEnv: TTypeEnv, exp: TExp): Boolean = exp match {
    case TExpLetIn(decs, exp, env) => {
      decs.foreach(_.walk(env, this))
      exp.walk(env, this)

      // TLetIn is specific to this pass. Do not let it get
      // walked normally.
      false
    }
    case TExpMatchRow(pat, exp, env) => {
      pat.foreach(_.walk(env, this))
      exp.walk(env, this)

      // The walk of the match row is specific to this. Do
      // not do the normal walk in addition.
      false
    }
    case other => true
  }
}

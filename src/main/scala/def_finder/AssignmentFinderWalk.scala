package def_finder

import tir._
import tpass.TPass

class AssignmentFinderWalk(ident: TNamedIdent)
    extends TPass[TTypeEnv, List[(TTypeEnv, TExpAssign)]] {
  def combine(x: List[(TTypeEnv, TExpAssign)],
              y: List[(TTypeEnv, TExpAssign)]) =
    x ::: y

  def default = List()

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case assignment @ TExpAssign(from, exp) =>
      if (from == ident)
        List((env, assignment)) ::: (super.apply(env, exp))
      else
        super.apply(env, assignment)
    case other =>
      super.apply(env, exp)
  }
}

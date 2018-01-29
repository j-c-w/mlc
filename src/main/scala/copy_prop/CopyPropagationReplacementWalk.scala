package copy_prop

import tir._
import tpass.TParentSetPass

class CopyPropagationReplacementWalk(oldValue: TNamedIdent,
                                     newValue: TExp,
                                     funLet: TExpFunLet, env: TTypeEnv)
    extends TParentSetPass[Unit] {
  override def apply(u: Unit, exp: TExp) = exp match {
    case TExpAssign(name, rvalue) => {
      if (name == oldValue) {
        assert(rvalue == newValue)
        // We do not have to walk the rhs of the expression because a variable
        // may not appear in its own definition.
        None
      } else {
        super.apply(u, exp)
      }
    }
    case TExpIdent(ident) =>
      if (ident == oldValue) {
        Some(newValue)
      } else {
        None
      }
    case _ => super.apply(u, exp)
  }
}

package recursion_analysis

import tir._
import tpass.TPass

class RecursionIdentifierWalk(name: TNamedIdent) extends TPass[Unit, Boolean] {
  override def combine(x: Boolean, y: Boolean) = x || y
  override def default = false

  override def apply(u: Unit, ident: TIdent) = ident match {
    case otherIdent: TNamedIdent =>
      otherIdent == name
    case other => super.apply(u, other)
  }
}

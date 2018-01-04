package alpha_rename

import scala.collection.mutable.HashMap
import tpass.TParentSetPass
import tir._

class AlphaApplyWalk(val map: HashMap[TNamedIdent, TNamedIdent])
    extends TParentSetPass[Unit] {
  override def apply(u: Unit, ident: TIdent) = ident match {
    case namedIdent: TNamedIdent =>
      Some(map(namedIdent))
    case other => super.apply(u, ident)
  }
}

package tir_utils

import tir._
import tpass.TParentSetPass
import scala.collection.mutable.Set

object AssignmentDeletionWalk extends TParentSetPass[Set[TNamedIdent]] {
  override def apply(namedSet: Set[TNamedIdent], exp: TExp) = exp match {
    case TExpAssign(assignName, assignExp) =>
      if (namedSet.contains(assignName)) {
        // We replace the store with a load of a unit.  This intuitively
        // what a store returns in any case.
        Some(TExpIdent(TUnitIdent()))
      } else {
        super.apply(namedSet, exp)
      }
    case other => super.apply(namedSet, exp)
  }
}

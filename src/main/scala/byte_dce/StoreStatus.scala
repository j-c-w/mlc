package byte_dce

import toplev.GenericPrintable

/* This set of case classes keep track of the status of a store
 * (i.e. if it is dead or not).
 *
 * Note that a dead store may be replaced by a pop if it accesses
 * mutable state.
 */
trait StoreStatus extends GenericPrintable

case object DeadStore extends StoreStatus {
  def prettyPrint = "Dead Store"
}

case object LiveStore extends StoreStatus {
  def prettyPrint = "Live Store"
}

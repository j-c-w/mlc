package tpass

import tir._

trait TTypeEnvUnitPass extends TTypeEnvPass[Unit] {
  override def default = ()
  override def combine(x: Unit, y: Unit) = x
  override def combineList(ls: List[Unit]) = ()
}

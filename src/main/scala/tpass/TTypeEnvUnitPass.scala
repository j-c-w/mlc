package tpass

import tir._

trait TTypeEnvUnitPass extends TTypeEnvPass[Unit] {
  def default = ()
  def combine(x: Unit, y: Unit) = x
}

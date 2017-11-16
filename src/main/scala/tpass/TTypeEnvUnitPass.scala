package tpass

import tir._

trait TTypeEnvPassUnit extends TTypeEnvPass[Unit] {
  def default = ()
  def combine(x: Unit, y: Unit) = x
}

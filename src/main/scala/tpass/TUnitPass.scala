package tpass

trait TUnitPass extends TPass[Unit, Unit] {
  def combine(u: Unit, v: Unit) = u
  def default = ()
}

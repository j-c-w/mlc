package tpass

trait TUnitPass extends TPass[Unit, Unit] {
  override def combine(u: Unit, v: Unit) = u
  override def combineList(u: List[Unit]) = ()
  override def default = ()
}

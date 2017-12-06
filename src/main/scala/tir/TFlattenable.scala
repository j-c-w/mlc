package tir

trait TFlattenable[T] {
  assert(this.isInstanceOf[T])
  def flatten: T
}

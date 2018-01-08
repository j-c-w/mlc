package tir

/* We require that T is an instance of the class in question.  */
trait TFlattenable[T] {
  def flatten: T
}

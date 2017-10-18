package exceptions

class UnreachableException(val reason: String)
    extends RuntimeException(reason) {
  def this() = this("")
}

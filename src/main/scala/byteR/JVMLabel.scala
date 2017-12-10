package byteR

import toplev.GenericPrintable

case class JVMLabel(var name: String) extends GenericPrintable {
  def prettyPrint = name
}

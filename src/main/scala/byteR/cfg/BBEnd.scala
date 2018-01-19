package byteR.cfg

import toplev.GenericPrintable

case class BBEnd(val index: Int) extends GenericPrintable {
  def prettyPrint = index.toString
}

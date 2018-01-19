package byteR.cfg

import toplev.GenericPrintable

case class BBStart(val index: Int) extends GenericPrintable {
  def prettyPrint = index.toString
}

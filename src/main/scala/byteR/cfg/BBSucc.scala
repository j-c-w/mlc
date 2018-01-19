package byteR.cfg

import toplev.GenericPrintable

case class BBSucc(val succs: List[BBStart]) extends GenericPrintable {
  def prettyPrint = succs.mkString(", ")
}

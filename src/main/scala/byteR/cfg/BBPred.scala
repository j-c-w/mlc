package byteR.cfg

import toplev.GenericPrintable

case class BBPred(val preds: List[BBStart]) extends GenericPrintable {
  def prettyPrint = preds.mkString(", ")
}

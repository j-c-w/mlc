package lower_tir

import byteR.JVMLabel
import scala.collection.mutable.HashMap

object LabelGenerator {
  var id: Int = 0
  var loopLabelMap = new HashMap[Int, JVMLabel]()

  def newLabel(): JVMLabel = {
    id += 1
    new JVMLabel("L" + id.toString)
  }

  def labelFor(loopID: Int) = {
    if (!loopLabelMap.contains(loopID)) {
      loopLabelMap(loopID) = newLabel
    }

    loopLabelMap(loopID)
  }
}

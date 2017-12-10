package lower_tir

import byteR.JVMLabel

object LabelGenerator {
  var id: Int = 0

  def newLabel(): JVMLabel = {
    id += 1
    new JVMLabel("L" + id.toString)
  }
}

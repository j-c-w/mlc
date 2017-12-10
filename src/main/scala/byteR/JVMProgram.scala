package byteR

import toplev.GenericPrintable

case class JVMProgram(var classes: List[JVMClass]) extends GenericPrintable {
  def prettyPrint = classes.map(_.prettyPrint).mkString("\n\n")
}

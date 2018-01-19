package byteR

import toplev.GenericPrintable

sealed trait JVMVariable extends GenericPrintable

case class JVMLocalVariable(number: Int) extends JVMVariable {
  def prettyPrint = "Local Number " + number
}

case class JVMStaticVariable(name: String) extends JVMVariable {
  def prettyPrint = "Static Variable " + name
}

case class JVMMemberVariable(name: String) extends JVMVariable {
  def prettyPrint = "Member Variable " + name
}

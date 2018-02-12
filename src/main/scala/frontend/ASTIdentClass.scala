package frontend

import toplev.GenericPrintable

sealed trait ASTIdentClass

case class ASTValClass() extends ASTIdentClass {
  def prettyPrint = "_val"
}

case class ASTFunClass() extends ASTIdentClass {
  def prettyPrint = "_fun"
}

case class ASTDataTypeClass() extends ASTIdentClass {
  def prettyPrint = "_datatype"
}

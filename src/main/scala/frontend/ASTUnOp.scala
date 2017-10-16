package frontend

import toplev.GenericPrintable

object ASTUnOp

sealed trait ASTUnOp extends GenericPrintable

case class ASTUnOpNegate() extends ASTUnOp {
  def prettyPrint = "~"
}

case class ASTUnOpNot() extends ASTUnOp {
  def prettyPrint = "not"
}

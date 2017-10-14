package frontend

import sext._

object ASTUnOp

sealed trait ASTUnOp {
  def prettyPrint: String
}

case class ASTUnOpNegate() extends ASTUnOp {
  def prettyPrint = "~"
}

case class ASTUnOpNot() extends ASTUnOp {
  def prettyPrint = "not"
}

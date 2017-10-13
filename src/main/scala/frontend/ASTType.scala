package frontend

import sext._

object ASTType

sealed trait ASTType {
  def prettyPrint: String
}

case class ASTTypeFunction(val arg: ASTType, val result: ASTType)
    extends ASTType {
  def prettyPrint = " %s -> %s ".format(arg, result)
}

// To avoid ambiguity, there must be at least two
// types in this type.
case class ASTTypeTuple(val args: List[ASTType]) extends ASTType {
  def prettyPrint = " " + args.mkString(" * ") + " "
}

sealed trait ASTTypeVar extends ASTType

case class ASTEqualityTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "''" + name
}

case class ASTUnconstrainedTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "'" + name
}

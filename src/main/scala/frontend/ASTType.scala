package frontend

import toplev.GenericPrintable

object ASTType

sealed trait ASTType extends GenericPrintable

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

case class ASTListType() extends ASTTypeVar {
  def prettyPrint = "list"
}

case class ASTIntType() extends ASTTypeVar {
  def prettyPrint = "int"
}

case class ASTRealType() extends ASTTypeVar {
  def prettyPrint = "real"
}

case class ASTBoolType() extends ASTTypeVar {
  def prettyPrint = "bool"
}

case class ASTStringType() extends ASTTypeVar {
  def prettyPrint = "string"
}

case class ASTCharType() extends ASTTypeVar {
  def prettyPrint = "char"
}

case class ASTDataTypeName(val name: String) extends ASTTypeVar {
  def prettyPrint = name
}

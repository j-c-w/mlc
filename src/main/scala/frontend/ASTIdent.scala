package frontend

import toplev.GenericPrintable

object ASTIdent

sealed trait ASTIdent extends GenericPrintable

case class ASTIdentVar(val id: String) extends ASTIdent {
  def prettyPrint = id
}

case class ASTLongIdent(val id: List[ASTIdent]) extends ASTIdent {
  def prettyPrint = (id map (_.prettyPrint)) mkString(".")
}

case class ASTIdentTuple(val ids: List[ASTIdent]) extends ASTIdent {
  def prettyPrint = (ids map (_.prettyPrint)) mkString(", ")
}

case class ASTUnderscoreIdent() extends ASTIdent {
  def prettyPrint = "_"
}

case class ASTConsIdent() extends ASTIdent {
  def prettyPrint = "::"
}

case class ASTEmptyListIdent() extends ASTIdent {
  def prettyPrint = "[]"
}

case class ASTUnitIdent() extends ASTIdent {
  def prettyPrint = "()"
}

case class ASTPlusIdent() extends ASTIdent {
  def prettyPrint = "+"
}

case class ASTMinusIdent() extends ASTIdent {
  def prettyPrint = "-"
}

case class ASTTimesIdent() extends ASTIdent {
  def prettyPrint = "*"
}

case class ASTIntDivIdent() extends ASTIdent {
  def prettyPrint = " div "
}

case class ASTRealDivIdent() extends ASTIdent {
  def prettyPrint = "/"
}

case class ASTModIdent() extends ASTIdent {
  def prettyPrint = " mod "
}

case class ASTAppendIdent() extends ASTIdent {
  def prettyPrint = "@"
}

case class ASTStringCatIdent() extends ASTIdent {
  def prettyPrint = "^"
}

case class ASTLTIdent() extends ASTIdent {
  def prettyPrint = "<"
}

case class ASTLEQIdent() extends ASTIdent {
  def prettyPrint = "<="
}

case class ASTGTIdent() extends ASTIdent {
  def prettyPrint = ">"
}

case class ASTGEQIdent() extends ASTIdent {
  def prettyPrint = ">="
}

case class ASTEqIdent() extends ASTIdent {
  def prettyPrint = "="
}

case class ASTAndIdent() extends ASTIdent {
  def prettyPrint = " andalso "
}

case class ASTOrIdent() extends ASTIdent {
  def prettyPrint = " orelse "
}

sealed trait ASTUnOp extends ASTIdent

case class ASTUnOpNegate() extends ASTUnOp {
  def prettyPrint = "~"
}

case class ASTUnOpNot() extends ASTUnOp {
  def prettyPrint = "not"
}

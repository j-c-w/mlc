package frontend

import sext._

object ASTIdent {
  // This requires a special constructor to simplify the grammar.
  def fromSpecialCharacter(c: String) = c match {
    case _ => ASTIdentVar(c)
  }
}

sealed trait ASTIdent {
  def prettyPrint: String
}

case class ASTIdentVar(val id: String) extends ASTIdent {
  def prettyPrint = id
}

case class ASTLongIdent(val id: List[ASTIdent]) extends ASTIdent {
  def prettyPrint = (id map (_.prettyPrint)) mkString(".")
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

case class ASTDivIdent() extends ASTIdent {
  def prettyPrint = "/"
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

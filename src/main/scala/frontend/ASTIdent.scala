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

case class Plus() extends ASTIdent {
  def prettyPrint = "+"
}

case class Minus() extends ASTIdent {
  def prettyPrint = "-"
}

case class Less() extends ASTIdent {
  def prettyPrint = "<"
}

case class Greater() extends ASTIdent {
  def prettyPrint = ">"
}

case class Equal() extends ASTIdent {
  def prettyPrint = "="
}

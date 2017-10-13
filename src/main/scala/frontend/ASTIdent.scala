package frontend

import sext._

object ASTIdent {
  // This requires a special constructor to simplify the grammar.
  def fromSpecialCharacter(c: String) = c match {
    case _ => {
      println("Matching c " + c)
      ASTIdentVar(c)
    }
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

// TODO -- Add a lot of case classes for the special cases of
// ASTIdent.

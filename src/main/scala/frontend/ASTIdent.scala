package frontend

import toplev.GenericPrintable

object ASTIdent

sealed trait ASTIdent extends GenericPrintable

sealed trait ASTInfixIdent extends ASTIdent {
  val precedence: Int
}

case class ASTIdentVar(val id: String) extends ASTIdent {
  def prettyPrint = id
}

case class ASTLongIdent(val id: List[ASTIdent]) extends ASTIdent {
  def prettyPrint = (id map (_.prettyPrint)) mkString(".")
}

case class ASTIdentTuple(val ids: List[ASTIdent]) extends ASTIdent {
  def prettyPrint = (ids map (_.prettyPrint)) mkString(", ")

  def flatten: ASTIdent =
    if (ids.length == 1)
      ids(0) match {
        case tuple: ASTIdentTuple => tuple.flatten
        case other => other
      }
    else
      this
}

case class ASTUnderscoreIdent() extends ASTIdent {
  def prettyPrint = "_"
}

case class ASTConsIdent() extends ASTInfixIdent {
  def prettyPrint = "::"

  val precedence = 5
}

case class ASTAppendIdent() extends ASTInfixIdent {
  def prettyPrint = "@"

  val precedence = 5
}

case class ASTEmptyListIdent() extends ASTIdent {
  def prettyPrint = "[]"
}

case class ASTUnitIdent() extends ASTIdent {
  def prettyPrint = "()"
}

case class ASTPlusIdent() extends ASTInfixIdent {
  def prettyPrint = "+"

  val precedence = 6
}

case class ASTMinusIdent() extends ASTInfixIdent {
  def prettyPrint = "-"

  val precedence = 6
}

case class ASTTimesIdent() extends ASTInfixIdent {
  def prettyPrint = "*"

  val precedence = 7
}

case class ASTIntDivIdent() extends ASTInfixIdent {
  def prettyPrint = " div "

  val precedence = 7
}

case class ASTRealDivIdent() extends ASTInfixIdent {
  def prettyPrint = "/"

  val precedence = 7
}

case class ASTModIdent() extends ASTInfixIdent {
  def prettyPrint = " mod "

  val precedence = 7
}

case class ASTStringCatIdent() extends ASTInfixIdent {
  def prettyPrint = "^"

  val precedence = 6
}

case class ASTLTIdent() extends ASTInfixIdent {
  def prettyPrint = "<"

  val precedence = 4
}

case class ASTLEQIdent() extends ASTInfixIdent {
  def prettyPrint = "<="

  val precedence = 4
}

case class ASTGTIdent() extends ASTInfixIdent {
  def prettyPrint = ">"

  val precedence = 4
}

case class ASTGEQIdent() extends ASTInfixIdent {
  def prettyPrint = ">="

  val precedence = 4
}

case class ASTEqIdent() extends ASTInfixIdent {
  def prettyPrint = "="

  val precedence = 4
}

case class ASTAndIdent() extends ASTInfixIdent {
  def prettyPrint = " andalso "

  val precedence = 0
}

case class ASTOrIdent() extends ASTInfixIdent {
  def prettyPrint = " orelse "

  val precedence = 0
}

sealed trait ASTUnOp extends ASTIdent

case class ASTUnOpNegate() extends ASTUnOp {
  def prettyPrint = "~"
}

case class ASTUnOpNot() extends ASTUnOp {
  def prettyPrint = "not"
}

case class ASTUnOpPrint() extends ASTUnOp {
  def prettyPrint = "print"
}

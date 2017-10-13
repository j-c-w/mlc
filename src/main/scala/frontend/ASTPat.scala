package frontend

import sext._

object ASTPat

sealed trait ASTPat {
  def prettyPrint: String
}

case class ASTPatWildcard() extends ASTPat {
  def prettyPrint = "_"
}

case class ASTPatVariable(val variable: ASTIdent) extends ASTPat {
  def prettyPrint = variable.prettyPrint
}

case class ASTPatSeq(val subseq: List[ASTPat]) extends ASTPat {
  def prettyPrint = "( " + (subseq map (_.prettyPrint)) mkString(", ") + " )"
}

// Note that this is distinct from ASTPatSeq.  This matches
// patterns written in list format.
case class ASTListPat(val listpat: List[ASTPat]) extends ASTPat {
  def prettyPrint = "[ " + (listpat map (_.prettyPrint)) mkString(", ") + " ]"
}

case class ASTPatConst(val const: ASTConst) extends ASTPat {
  def prettyPrint = const prettyPrint
}

case class ASTPatTyped(val pat: ASTPat, val typ: ASTType) extends ASTPat {
  def prettyPrint = "%s : %s".format(pat.prettyPrint, typ.prettyPrint)
}

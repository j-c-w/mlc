package frontend

import sext._

object ASTPat

sealed trait ASTPat {
  def prettyPrint: String
}

case class ASTPatWildcard(val typ: List[ASTType]) extends ASTPat {
  def prettyPrint = "_"
}

case class ASTPatVariable(val variable: ASTIdent, val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = variable.prettyPrint
}

case class ASTPatSeq(val subseq: List[ASTPat], val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = "( " + (subseq map (_.prettyPrint)).mkString(", ") + " )"
}

// Note that this is distinct from ASTPatSeq.  This matches
// patterns written in list format.
case class ASTListPat(val listpat: List[ASTPat], val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = "[ " + (listpat map (_.prettyPrint)).mkString(", ") + " ]"
}

case class ASTPatConst(val const: ASTConst, val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = const prettyPrint
}

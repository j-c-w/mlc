package frontend

import toplev.GenericPrintable

object ASTPat

sealed trait ASTPat extends GenericPrintable

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

case class ASTPatCons(val head: ASTPat, val tail: ASTPat) extends ASTPat {
  def prettyPrint = "( %s :: %s )".format(head.prettyPrint, tail.prettyPrint)
}

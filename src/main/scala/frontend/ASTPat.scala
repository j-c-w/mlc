package frontend

import toplev.GenericPrintable

object ASTPat

sealed trait ASTPat extends GenericPrintable {
  // This is for combining empty types
  def appendTypes(typ: List[ASTType]): ASTPat
}

case class ASTPatWildcard(val typ: List[ASTType]) extends ASTPat {
  def prettyPrint = "_"

  def appendTypes(typs: List[ASTType]) =
    new ASTPatWildcard(typ ::: typs)
}

case class ASTPatVariable(val variable: ASTIdent, val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = variable.prettyPrint
  
  def appendTypes(typs: List[ASTType]) =
    new ASTPatVariable(variable, typ ::: typs)
}

case class ASTPatSeq(val subseq: List[ASTPat], val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = "( " + (subseq map (_.prettyPrint)).mkString(", ") + " )"

  def appendTypes(typs: List[ASTType]) =
    new ASTPatSeq(subseq, typ ::: typs)
}

// Note that this is distinct from ASTPatSeq.  This matches
// patterns written in list format.
case class ASTListPat(val listpat: List[ASTPat], val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = "[ " + (listpat map (_.prettyPrint)).mkString(", ") + " ]"

  def appendTypes(typs: List[ASTType]) =
    new ASTListPat(listpat, typ ::: typs)
}

case class ASTPatConst(val const: ASTConst, val typ: List[ASTType])
    extends ASTPat {
  def prettyPrint = const prettyPrint

  def appendTypes(typs: List[ASTType]) =
    new ASTPatConst(const, typ ::: typs)
}

case class ASTPatCons(val head: ASTPat, val tail: ASTPat,
                      val typ: List[ASTType]) extends ASTPat {
  def prettyPrint = "( %s :: %s )".format(head.prettyPrint, tail.prettyPrint)

  def appendTypes(typs: List[ASTType]) =
    ASTPatCons(head, tail, typ ::: typs)
}

case class ASTPatConstructor(val constructor: ASTIdent,
                             val args: Option[ASTPat],
                             val typ: List[ASTType]) extends ASTPat {
  def prettyPrint = args match {
    case Some(args) =>
      "%s(%s)".format(constructor.prettyPrint, args.prettyPrint)
    case None => constructor.prettyPrint
  }

  def appendTypes(typs: List[ASTType]) =
    ASTPatConstructor(constructor, args, typ ::: typs)
}

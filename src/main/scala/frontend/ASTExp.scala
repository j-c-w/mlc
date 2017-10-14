package frontend

import sext._

object ASTExp

sealed trait ASTExp {
  def prettyPrint: String
}

case class ASTExpConst(val const: ASTConst) extends ASTExp {
  def prettyPrint = const.prettyPrint
}

case class ASTExpIdent(val ident: ASTIdent) extends ASTExp {
  def prettyPrint = ident.prettyPrint
}

case class ASTExpFunApp(val fun: ASTExp, val app: ASTExp) extends ASTExp {
  def prettyPrint = "(" + fun.prettyPrint + ")" + "(" + app + ")"
}

case class ASTExpInfixApp(val operator: ASTIdent, val operand1: ASTExp,
                          val operand2: ASTExp) extends ASTExp {
  def prettyPrint = operand1.prettyPrint + " " + operator.prettyPrint + " " + 
      operand2.prettyPrint
}

case class ASTExpUnOpApply(val operator: ASTUnOp, val operand: ASTExp)
    extends ASTExp {
  def prettyPrint = operator.prettyPrint + " (" + operand.prettyPrint + ")"
}


case class ASTExpTuple(val elems: List[ASTExp]) extends ASTExp {
  def prettyPrint = "(" + (elems map (_.prettyPrint)).mkString(", ") +  ")"
}

case class ASTExpList(val elems: List[ASTExp]) extends ASTExp {
  def prettyPrint = "[" + (elems map (_.prettyPrint)).mkString(", ") +  "]"
}

case class ASTExpLetIn(val dec: List[ASTDeclaration], val exp: List[ASTExp])
    extends ASTExp {
  def prettyPrint = """
  let
    %s
  in
    %s
  end
  """.format((dec map (_.prettyPrint)).mkString(";\n"),
             (exp map (_.prettyPrint)).mkString(";\n"))
}

case class ASTExpSeq(val seq: List[ASTExp]) extends ASTExp {
  def prettyPrint = (seq map (_.prettyPrint)).mkString(";\n")
}

case class ASTExpTyped(val exp: ASTExp, val typ: ASTType) extends ASTExp {
  def prettyPrint = " %s : %s ".format(exp.prettyPrint, typ.prettyPrint)
}

case class ASTExpOr(val e1: ASTExp, val e2: ASTExp) extends ASTExp {
  def prettyPrint = " ( %s orelse %s ) ".format(e1.prettyPrint, e2.prettyPrint)
}

case class ASTExpAnd(val e1: ASTExp, val e2: ASTExp) extends ASTExp {
  def prettyPrint = " ( %s andalso %s ) ".format(e1.prettyPrint, 
                                                 e2.prettyPrint)
}

case class ASTExpIfThenElse(val cond: ASTExp, val taken: ASTExp,
                            val notTaken: ASTExp) extends ASTExp {
  def prettyPrint = """
  ( if %s then
      %s
    else
      %s)
  """.format(cond.prettyPrint, taken.prettyPrint, notTaken.prettyPrint)
}

case class ASTExpCase(val exp: ASTExp, val caseList: List[ASTExpMatchRow])
    extends ASTExp {
  def prettyPrint = """ ( match %s with %s ) """.format(exp.prettyPrint,
        (caseList map (_.prettyPrint)).mkString("\n| "))
}

case class ASTExpMatchRow(val pat: ASTPat, val exp: ASTExp) extends ASTExp {
  def prettyPrint = """ case %s => %s """.format(pat.prettyPrint,
                                                 exp.prettyPrint)
}

case class ASTExpFn(val body: List[ASTExpMatchRow]) extends ASTExp {
  def prettyPrint = """(fn %s)""".format((body map (_.prettyPrint)).
      mkString("\n| "))
}

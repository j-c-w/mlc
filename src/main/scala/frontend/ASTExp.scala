package frontend

import exceptions.ICE
import toplev.GenericPrintable

object ASTExp

sealed trait ASTExp extends GenericPrintable

case class ASTExpConst(val const: ASTConst) extends ASTExp {
  def prettyPrint = const.prettyPrint
}

case class ASTExpIdent(val ident: ASTIdent) extends ASTExp {
  def prettyPrint = ident.prettyPrint
}

case class ASTExpFunApp(val fun: ASTExp, val app: ASTExp) extends ASTExp {
  def prettyPrint = "(" + fun.prettyPrint + ")" + "(" + app.prettyPrint + ")"

  /* Due to the design of the grammar, it is hard to make a left associative
   * function application operator. Therefore, a method to do this
   * is provided here. For safety, if this is called after the callType
   * has been set (set in typechecking), it throws.
   *
   * This takes something like this:
   *
   *      f, ((1 2) 3) -> (((f 1) 2) 3)
   */
  def leftAssociate(newFun: ASTExp): ASTExpFunApp =
    if (callType != None)
      throw new ICE("""leftAssociate called on an already typed function""")
    else {
      fun match {
        case fun @ ASTExpFunApp(appFun, appApp) =>
          ASTExpFunApp(fun.leftAssociate(newFun), app)
        case value => ASTExpFunApp(ASTExpFunApp(newFun, fun), app)
      }
    }

  // This stores the call type. It is set by the type inference.
  // For example, if this is +(1,  2), then the type
  // becomes (int * int) -> int.
  var callType: Option[ASTTypeFunction] = None
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
  // Nodes that restrict scope must have their own typing environments.
  // These are filled during the typecheck pass.
  var typeEnv: Option[ASTTypeEnv] = None

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
  var env: Option[ASTTypeEnv] = None

  def prettyPrint = """ case %s => %s """.format(pat.prettyPrint,
                                                 exp.prettyPrint)
}

case class ASTExpFn(val body: List[ASTExpMatchRow]) extends ASTExp {
  def prettyPrint = """(fn %s)""".format((body map (_.prettyPrint)).
      mkString("\n| "))
}

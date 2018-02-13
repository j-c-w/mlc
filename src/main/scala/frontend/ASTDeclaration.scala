package frontend

import toplev.GenericPrintable

object ASTDeclaration

// Top level definitions
sealed trait ASTDeclaration extends GenericPrintable {
  def getIdent: ASTIdent
}

case class ASTValBind(val ident: ASTIdent, val expression: ASTExp)
    extends ASTDeclaration {
  def prettyPrint = """

  val %s = %s

  """.format(ident.prettyPrint, expression.prettyPrint)

  def getIdent = ident
}

case class ASTFunBind(val cases: List[(ASTIdent, List[ASTPat],
                                       Option[ASTType], ASTExp)])
      extends ASTDeclaration {
  var rowEnvs: Option[List[ASTTypeEnv]] = None

  def prettyPrint = """

    fun %s

    """.format((cases map {
                  case (id, pat, None, exp) =>
                    "| " + id.prettyPrint + " " + (pat map (_.prettyPrint)).
                    mkString(" ") + " = " + exp.prettyPrint
                  case (id, pat, Some(typ), exp) => "| " + id.prettyPrint +
                    " " + (pat map (_.prettyPrint)).mkString(" ") + " : " +
                    typ.prettyPrint + " = " + exp.prettyPrint
               }).mkString("\n"))

  def getIdent = {
    assert(cases.length > 0)

    // Note that this may be called before typechecking.
    // Therefore, we may not assume that all the function names
    // are the same.
    cases(0)._1
  }
}

case class ASTExceptionBind(val ident: ASTIdent, val typ: Option[ASTType])
    extends ASTDeclaration {
  def prettyPrint = 
    typ match {
      case Some(typ) => """
        |exception %s of (%s)
        |""".stripMargin.format(ident.prettyPrint, typ.prettyPrint)
      case None => "\nexception %s\n".format(ident.prettyPrint)
    }

  def getIdent = ident

}

// Datatype definitions
case class ASTDataTypeBind(val ident: ASTIdent, val typ: Option[ASTType],
                           val dataClass: ASTDataType)
    extends ASTDeclaration {
  def prettyPrint =
    typ match {
      case Some(typ) => """
      |datatype %s: %s of %s
      |""".stripMargin.format(ident.prettyPrint, dataClass.prettyPrint,
                              typ.prettyPrint)
      case None =>
        """
        |datatype %s: %s
        |""".stripMargin.format(ident.prettyPrint, dataClass.prettyPrint)
    }

  def getIdent = ident
}

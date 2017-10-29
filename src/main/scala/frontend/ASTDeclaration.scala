package frontend

import toplev.GenericPrintable

object ASTDeclaration

// Top level definitions
sealed trait ASTDeclaration extends GenericPrintable {
  def getIdent: ASTIdent
}

case class ASTValBind(val ident: ASTIdentTuple, val expression: ASTExp)
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

// Datatype definitions
sealed trait ASTDataConstructor {
  def prettyPrint: String
}

case class ASTDataType(val ident: ASTIdent,
                       val classes: List[ASTDataConstructor])
                       extends ASTDeclaration {
  def prettyPrint = """

  datatype %s = %s

  """.format(ident.prettyPrint, (classes map(_.prettyPrint)).mkString(" | "))

  def getIdent = ident
}

case class ASTDataConstructorDefinition(val ident: ASTIdent)
                                        extends ASTDataConstructor {
  def prettyPrint = ident.prettyPrint

  def getIdent = ident
}

case class ASTDataConstructorDefinitionWithType(val ident: ASTIdent,
                                                val classType: ASTType)
                                                extends ASTDataConstructor {
  def prettyPrint = ident.prettyPrint + " of " + classType.prettyPrint

  def getIdent = ident
}

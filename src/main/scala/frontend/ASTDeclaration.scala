package frontend

import sext._

object ASTDeclaration

// Top level definitions
sealed trait ASTDeclaration {
  def prettyPrint: String
}

case class ASTValBind(val ident: ASTIdent, val expression: ASTExp)
    extends ASTDeclaration {
  def prettyPrint = """
  val %s = %s
  """.format(ident.prettyPrint, expression.prettyPrint)
}

sealed trait ASTFun extends ASTDeclaration

case class ASTFunBind(cases: List[(ASTIdent, List[ASTPat],
                                   Option[ASTType], ASTExp)]) extends ASTFun {
    def prettyPrint = """

    fun %s
    """.format(cases map {
                  case (id, pat, None, exp) =>
                    "| " + id.prettyPrint + " " + (pat map (_.prettyPrint)).
                    mkString(" ") + " = " + exp.prettyPrint
                  case (id, pat, Some(typ), exp) => "| " + id.prettyPrint +
                    " " + (pat map (_.prettyPrint)).mkString(" ") + " : " +
                    typ.prettyPrint + " = " + exp.prettyPrint
               }).mkString("\n")
}

// Datatype definitions
sealed trait ASTDataConstructor extends ASTDeclaration {
  def prettyPrint: String
}

case class ASTDataType(val ident: ASTIdent,
                       val classes: List[ASTDataConstructor])
                       extends ASTDataConstructor {
  def prettyPrint = """
  datatype %s = %s
  """.format(ident.prettyPrint, (classes map(_.prettyPrint)).mkString(" | "))
}

case class ASTDataConstructorDefinition(val ident: ASTIdent)
                                        extends ASTDataConstructor {
  def prettyPrint = ident.prettyPrint
}

case class ASTDataConstructorDefinitionWithType(val ident: ASTIdent,
                                                val classType: ASTType)
                                                extends ASTDataConstructor {
  def prettyPrint = ident.prettyPrint + " of " + classType
}

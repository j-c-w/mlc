package frontend

import toplev.GenericPrintable

object ASTProgram

class ASTProgram(val decs: List[ASTDeclaration]) extends GenericPrintable {
  def prettyPrint = 
    (decs map (_.prettyPrint)).mkString("\n")

  override def toString =
    (decs map (_.toString)).mkString("\n")
}

class TypedASTProgram(decs: List[ASTDeclaration],
                      val env: ASTTypeEnv) extends ASTProgram(decs) {
  override def prettyPrint = """
  %s

  =====

  %s
  """.format(env.prettyPrint, super.prettyPrint)

  override def toString = """
  %s

  =======

  %s
  """.format(env.prettyPrint, super.toString)

}

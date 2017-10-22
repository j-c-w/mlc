package frontend

import toplev.GenericPrintable

object ASTProgram

class ASTProgram(val decs: List[ASTDeclaration]) extends GenericPrintable {
  var env: Option[ASTTypeEnv] = None

  def prettyPrint = {
    (decs map (_.prettyPrint)).mkString("\n")
  }

  override def toString =
    (decs map (_.toString)).mkString("\n")
}

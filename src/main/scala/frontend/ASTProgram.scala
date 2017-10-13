package frontend

import sext._

object ASTProgram

class ASTProgram(val decs: List[ASTDeclaration]) {
  def prettyPrint = 
    (decs map (_.prettyPrint)).mkString("\n")
}

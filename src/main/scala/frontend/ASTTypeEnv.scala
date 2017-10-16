package frontend

import toplev.GenericTypeEnv

class ASTTypeEnv(parent: Option[ASTTypeEnv])
    extends GenericTypeEnv[ASTTypeEnv, ASTIdent, ASTType](parent) {
  def this() = this(None)
}

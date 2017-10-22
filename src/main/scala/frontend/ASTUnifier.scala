package frontend

import toplev.GenericUnifier

object ASTUnifier {
  def apply() = new ASTUnifier()
}

class ASTUnifier extends GenericUnifier[ASTType] {
  // See the comment on the abstract definition of this.
  override def isValidSpecialization(from: ASTType, to: ASTType) = {
    from.isAtomic && ASTType.isValidSpecialization(from, to)
  }

  override def unify(t: ASTType, u: ASTType) =
    ASTType.unify(t, u)
}

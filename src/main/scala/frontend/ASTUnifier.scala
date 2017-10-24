package frontend

import toplev.GenericUnifier

object ASTUnifier {
  def apply() = new ASTUnifier()
  def apply(from: ASTType, to: ASTType) = {
    val unifier = new ASTUnifier()
    unifier.specializeNV(from, to)
    unifier
  }
}

class ASTUnifier extends GenericUnifier[ASTType] {
  // See the comment on the abstract definition of this.
  override def isValidSpecialization(from: ASTType, to: ASTType) = {
    from.isAtomic && ASTType.isValidSpecialization(from, to)
  }

  override def specializeTo(from: ASTType, to: ASTType) = {
    from specializeTo to
  }

  override def unifyTo(from: ASTType, to: ASTType) = {
    from unify to
  }
}

package frontend

import toplev.GenericTypeEnv
import typecheck.TypeVariableGenerator

class ASTTypeEnv(parent: Option[ASTTypeEnv])
    extends GenericTypeEnv[ASTTypeEnv, ASTIdent, ASTType](parent) {
  def this() = this(None)

  override def get(ident: ASTIdent) = ident match {
    case ASTLongIdent(names) => ??? // To do, implement the standard
    // libraries for this case. Unlikely to want to put those here,
    // they can go in their own package
    case ASTIdentTuple(_) => None // This is not handled here.
    case ASTUnderscoreIdent() => Some(TypeVariableGenerator.getVar())
    case ASTConsIdent() => {
      val typ = TypeVariableGenerator.getVar()

      Some(ASTTypeFunction(
        ASTTypeTuple(
          List(typ,
            ASTListType(typ))),
        ASTListType(typ)))
    }
    case ASTEmptyListIdent() =>
      Some(ASTListType(TypeVariableGenerator.getVar()))
    case ASTMinusIdent() | ASTPlusIdent() |
         ASTTimesIdent() | ASTDivIdent() => {
      val numType = TypeVariableGenerator.getNumberTypeVar()
      Some(ASTTypeFunction(
        ASTTypeTuple(
          List(numType, numType)),
        numType))
    }
    // case ASTUnitIdent() => ASTUnitType()
    case x => super.get(x)
  }
}

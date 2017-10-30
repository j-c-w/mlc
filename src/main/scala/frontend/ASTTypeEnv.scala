package frontend

import exceptions.ICE
import stdlib.StandardLibraries
import toplev.GenericTypeEnv
import typecheck.TypeVariableGenerator

class ASTTypeEnv(parent: Option[ASTTypeEnv])
    extends GenericTypeEnv[ASTTypeEnv, ASTIdent, ASTType](parent) {
  def this() = this(None)

  override def get(ident: ASTIdent) = ident match {
    case ASTLongIdent(names) => {
      StandardLibraries(names map {
        case ASTIdentVar(name) => name
        case ident => throw new ICE("""  Long Ident composed of non string 
          components: %s """ + ident.prettyPrint)
      })
    }
    case ASTIdentTuple(_) => None // This is not handled here.
    case ASTUnderscoreIdent() => Some(TypeVariableGenerator.getVar())
    case ASTConsIdent() => {
      val typ = TypeVariableGenerator.getVar()

      Some(ASTFunctionType(
        ASTTupleType(
          List(typ,
            ASTListType(typ))),
        ASTListType(typ)))
    }
    case ASTEmptyListIdent() =>
      Some(ASTListType(TypeVariableGenerator.getVar()))
    case ASTUnitIdent() => Some(ASTUnitType())
    case ASTMinusIdent() | ASTPlusIdent() |
         ASTTimesIdent() => {
      val numType = TypeVariableGenerator.getNumberTypeVar()
      Some(ASTFunctionType(
        ASTTupleType(
          List(numType, numType)),
        numType))
    }
    case ASTIntDivIdent() =>
      Some(ASTFunctionType(
        ASTTupleType(List(
          ASTIntType(),
          ASTIntType())),
      ASTIntType()))
  case ASTRealDivIdent() =>
    Some(ASTFunctionType(
      ASTTupleType(List(
        ASTRealType(),
        ASTRealType())),
    ASTRealType()))
    case ASTModIdent() =>
      Some(ASTFunctionType(
        ASTTupleType(List(
          ASTIntType(), ASTIntType())),
      ASTIntType()))
    case ASTAppendIdent() => {
      val listType = TypeVariableGenerator.getVar()

      Some(ASTFunctionType(
        ASTTupleType(List(
          ASTListType(listType),
          ASTListType(listType))),
        ASTListType(listType)))
    }
    case ASTStringCatIdent() =>
      Some(ASTFunctionType(
        ASTTupleType(List(
          ASTStringType(),
          ASTStringType())),
        ASTStringType()))
    case ASTLEQIdent() | ASTLTIdent() |
         ASTGTIdent()  | ASTGEQIdent() => {
      val numType = TypeVariableGenerator.getNumberTypeVar()

      Some(ASTFunctionType(
        ASTTupleType(List(
          numType,
          numType)),
        ASTBoolType()))
    }
    case ASTEqIdent() => {
      val eqType = TypeVariableGenerator.getEqualityVar()

      Some(ASTFunctionType(
        ASTTupleType(List(
          eqType,
          eqType)),
        ASTBoolType()))
    }
    case ASTUnOpNegate() => {
      val numType = TypeVariableGenerator.getNumberTypeVar()
      Some(ASTFunctionType(numType, numType))
    }
    case ASTUnOpNot() => {
      Some(ASTFunctionType(
        ASTBoolType(), ASTBoolType()))
    }
    case x => super.get(x) match {
      case Some(y) => Some(y)
      case None =>
        // There are some things, like 'print', that we have to match
        // last.
        x match {
          case ASTIdentVar("print") =>
            Some(ASTFunctionType(ASTStringType(), ASTUnitType()))
          case _ => None
        }
    }
  }
}

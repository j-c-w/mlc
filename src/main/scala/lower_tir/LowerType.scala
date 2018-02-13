package lower_tir

import exceptions.ICE
import tir._
import byteR._

object LowerType {
  def apply(typ: TType, env: TTypeEnv): JVMType = typ match {
    case TFunctionType(_, _) => JVMFunctionType()
    case TTupleType(subTypes) => JVMTupleType()
    case TEqualityTypeVar(name) => JVMObjectType()
    case TUnconstrainedTypeVar(name) => JVMObjectType()
    case TListType(subType) => JVMLinkedListType()
    case TExceptionType() => JVMExceptionType()
    case TDataTypeInstance(name) => {
      name match {
        case TIdentVar(id, _) =>
          JVMDataTypeParentClass(
            JVMClassRef.classRefFor("DataTypeParentClass" + LowerName(id)))
        case TTopLevelIdent(id, _) =>
          JVMDataTypeType(
            JVMClassRef.classRefFor("DataTypeClass" + LowerName(id)))
        case other => throw new ICE("Unrecognized ident type " + name)
      }
    }
    case TDataType(name) =>
      JVMDataTypeParentClass(
        JVMClassRef.classRefFor("DataTypeParentClass" + LowerName(name)))
    case TIntType() => JVMIntegerType()
    case TStringType() => JVMStringType()
    case TRealType() => JVMFloatType()
    case TBoolType() => JVMBooleanType()
    case TCharType() => JVMCharacterType()
    case TUnitType() => JVMUnitType()
  }
}

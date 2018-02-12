package lower_tir

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
    case TDataType(name) => {
      // Note that data types are stored as functions.
      val dataTyp = env.getOrFail(name) match {
        case TFunctionType(from, to) => to
        case other => other
      }

      JVMDataTypeType(
        JVMClassRef.classRefFor(
          "DatatypeClass" + LowerName(name.asInstanceOf[TTopLevelIdent].name)))
    }
    case TIntType() => JVMIntegerType()
    case TStringType() => JVMStringType()
    case TRealType() => JVMFloatType()
    case TBoolType() => JVMBooleanType()
    case TCharType() => JVMCharacterType()
    case TUnitType() => JVMUnitType()
  }
}

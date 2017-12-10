package lower_tir

import tir._
import byteR._

object LowerType {
  def apply(typ: TType): JVMType = typ match {
    case TFunctionType(_, _) => JVMFunctionType()
    case TTupleType(subTypes) => JVMTupleType()
    case TEqualityTypeVar(name) => JVMObjectType()
    case TUnconstrainedTypeVar(name) => JVMObjectType()
    case TListType(subType) => JVMLinkedListType()
    case TIntType() => JVMIntegerType()
    case TStringType() => JVMStringType()
    case TRealType() => JVMFloatType()
    case TBoolType() => JVMBooleanType()
    case TCharType() => JVMCharacterType()
    case TUnitType() => JVMUnitType()
  }
}

package lower_tir

import tir._
import byteR._

object LowerConst {
  def apply(const: TConst): List[JVMInstruction] = const match {
      case TConstInt(value) => {
        // First get the code for loading the value:
        List(JVMIPush(value), LowerExp.box(JVMIntPrimitiveType()))
      }
      case TConstFloat(value) =>
        List(JVMFPush(value), LowerExp.box(JVMFloatPrimitiveType()))
      case TConstString(name) => {
        List(JVMLDCString(LowerString(name)))
      }
      case TConstChar(character) =>
        List(JVMIPush(character.toInt),
             LowerExp.box(JVMCharPrimitiveType()))
      case TConstTrue() =>
        List(JVMIPush(1), LowerExp.box(JVMBooleanPrimitiveType()))
      case TConstFalse() =>
        List(JVMIPush(0), LowerExp.box(JVMBooleanPrimitiveType()))
  }
}

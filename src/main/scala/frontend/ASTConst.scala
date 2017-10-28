package frontend

import java.math.BigInteger
import java.math.BigDecimal
import toplev.GenericPrintable

object ASTConst

sealed trait ASTConst extends GenericPrintable {
  def getType(): ASTType
}

// We use the really large formats so that the compiler
// doesn't crash on input.
case class ASTConstInt(val int: BigInteger) extends ASTConst {
  def prettyPrint = int.toString()

  def getType = ASTIntType()
}

// The ASTConstFloat class requires a companion object to 
// provide a special constructor.
object ASTConstFloat {
  def apply(intPart: ASTConstInt, decimalPart: ASTConstInt,
           exponent: ASTConstInt) = {
    new ASTConstFloat(new BigDecimal(1.0f))
  }
}

case class ASTConstFloat(val float: BigDecimal) extends ASTConst {
  def prettyPrint = float.toString()

  def getType = ASTRealType()
}

case class ASTConstString(val string: String) extends ASTConst {
  def prettyPrint = "\"" + string + "\""

  def getType = ASTStringType()
}

case class ASTConstChar(val char: Char) extends ASTConst {
  def prettyPrint = "#" + char

  def getType = ASTCharType()
}

sealed trait ASTConstBool extends ASTConst {
  def getType = ASTBoolType()
}

case class ASTConstTrue() extends ASTConstBool {
  def prettyPrint = "true"
}

case class ASTConstFalse() extends ASTConstBool {
  def prettyPrint = "false"
}

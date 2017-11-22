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
  def apply(intPart: ASTConstInt, decimalPart: (Int, ASTConstInt),
            exponent: ASTConstInt) = {
    var const = new BigDecimal(intPart.int)

    // Shift this left by the length of the decimal part,
    // add the decimal part, then shift right again.
    const = const.movePointRight(decimalPart._1)
    const = const.add(new BigDecimal(decimalPart._2.int))
    const = const.movePointLeft(decimalPart._1)

    // If the exponent is larger than IntMax, then the result won't fit in
    // a double, so we just use +- IntMax as a cap.
    val exponentInt = exponent.int.intValue()
    val power =
      if (exponent.int.compareTo(BigInteger.valueOf(exponentInt)) == 0
          && Math.abs(exponentInt) < 1000000)
        exponentInt
      else
        1000000 * exponent.int.signum()

    const = const.scaleByPowerOfTen(power)

    new ASTConstFloat(const)
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

package lexer

import java.math.{BigDecimal,BigInteger}
import toplev.GenericPrintable

sealed trait Lexeme extends GenericPrintable {
  def prettyPrint = toString
}

sealed trait LexemeKeyword extends Lexeme

// The LexFloatLiteral class requires a companion object to
// provide a special constructor.
object LexFloatLiteral {
  def apply(intPart: LexIntLiteral, decimalPart: (Int, LexIntLiteral),
            exponent: LexIntLiteral) = {
    // The below math assumes that the value is positive
    var const = new BigDecimal(intPart.value.abs())
    var signum = intPart.value.signum()

    // Shift this left by the length of the decimal part,
    // add the decimal part, then shift right again.
    const = const.movePointRight(decimalPart._1)
    const = const.add(new BigDecimal(decimalPart._2.value))
    const = const.movePointLeft(decimalPart._1)

    // If the exponent is larger than IntMax, then the result won't fit in
    // a double, so we just use +- IntMax as a cap.
    val exponentInt = exponent.value.intValue()
    val power =
      if (exponent.value.compareTo(BigInteger.valueOf(exponentInt)) == 0
          && Math.abs(exponentInt) < 1000000)
        exponentInt
      else
        1000000 * exponent.value.signum()

    const = const.scaleByPowerOfTen(power)
    // And return the sign to the number:
    if (signum == -1) {
      const = const.negate()
    }

    new LexFloatLiteral(const)
  }
}

case class LexCharLiteral(value: Char) extends Lexeme
case class LexFloatLiteral(value: BigDecimal) extends Lexeme
case class LexIntLiteral(value: BigInteger) extends Lexeme
case class LexStringLiteral(value: String) extends Lexeme
case object LexBoolFalse extends Lexeme
case object LexBoolTrue extends Lexeme

case class LexIdentifier(name: String) extends Lexeme
case class LexLongIdentifier(name: List[String]) extends Lexeme
case object LexColon extends Lexeme
case object LexComma extends Lexeme
case object LexComment extends Lexeme
case object LexEq extends Lexeme
case object LexSemiColon extends Lexeme
case object LexUnderscore extends Lexeme
case object LexVBar extends Lexeme

/* For lists.  */
case object LexLBrack extends Lexeme
case object LexRBrack extends Lexeme
case object LexNil extends LexemeKeyword

case object LexLParen extends Lexeme
case object LexRParen extends Lexeme

/* Declarations.  */
case object LexDatatype extends LexemeKeyword
case object LexException extends LexemeKeyword
case object LexFun extends LexemeKeyword
case object LexVal extends LexemeKeyword

case object LexFn extends LexemeKeyword
case object LexFnDecArrow extends Lexeme
// inttype and dot not generated and tident funtype

/* Types.  */
case object LexBoolType extends LexemeKeyword
case object LexCharType extends LexemeKeyword
case object LexExceptionType extends LexemeKeyword
case object LexFunType extends Lexeme
case object LexIntType extends LexemeKeyword
case object LexListType extends LexemeKeyword
case object LexRealType extends LexemeKeyword
case object LexStringType extends LexemeKeyword
case object LexUnitType extends LexemeKeyword
case class LexUnconstrainedType(val name: String) extends Lexeme
case class LexEqualityType(val name: String) extends Lexeme

/* Case statements.  */
case object LexCase extends LexemeKeyword
case object LexOf extends LexemeKeyword

/* If statements.  */
case object LexIf extends LexemeKeyword
case object LexThen extends LexemeKeyword
case object LexElse extends LexemeKeyword

/* Let ... in ... end. */
case object LexLet extends LexemeKeyword
case object LexIn extends LexemeKeyword
case object LexEnd extends LexemeKeyword

/* Raise and Handle.  */
case object LexHandle extends LexemeKeyword
case object LexRaise extends LexemeKeyword

/* Boolean operations.  */
case object LexOrElse extends LexemeKeyword
case object LexAndAlso extends LexemeKeyword
case object LexNot extends LexemeKeyword

/* Arithmetic operations.  */
case object LexIntDiv extends LexemeKeyword
case object LexMinus extends Lexeme
case object LexMod extends LexemeKeyword
case object LexNeg extends LexemeKeyword
case object LexPlus extends Lexeme
case object LexRealDiv extends Lexeme
case object LexTimes extends Lexeme

/* String concatenation.  */
case object LexStringCat extends Lexeme

/* Comparison operations.  */
case object LexGEQ extends Lexeme
case object LexGT extends Lexeme
case object LexLEQ extends Lexeme
case object LexLT extends Lexeme
case object LexNeq extends Lexeme

/* For lists.  */
case object LexCons extends Lexeme
case object LexAppend extends Lexeme

/* Values and built-in idents.  */
case object LexPrint extends Lexeme
case object LexUnit extends Lexeme

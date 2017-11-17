package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TIdent extends GenericPrintable

sealed trait BuiltinIdent extends TIdent

case class TIdentTuple(var subTypes: List[TIdent]) extends TIdent {
  def prettyPrint = "(" + subTypes.map(_.prettyPrint).mkString(", ") + ")"
}

case class TIdentVar(var name: String) extends TIdent {
  def prettyPrint = name
}

case class TIdentLongVar(var name: List[String]) extends TIdent {
  def prettyPrint = name.mkString(".")
}

/* This class is introduced in the LowerProgram pass.  */
case class TArgumentNode(var argNumber: Int) extends TIdent {
  def prettyPrint = "Argument_" + argNumber
}

case class TUnderscoreIdent() extends BuiltinIdent {
  def prettyPrint = "_"
}

case class TUnitIdent() extends BuiltinIdent {
  def prettyPrint = "()"
}

case class TConsIdent() extends BuiltinIdent {
  def prettyPrint = "::"
}

case class TAppendIdent() extends BuiltinIdent {
  def prettyPrint = "@"
}

case class TEmptyListIdent() extends BuiltinIdent {
  def prettyPrint = "[]"
}

case class TRealPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +r "
}

case class TRealMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -r "
}

case class TRealTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *r "
}

case class TRealDivIdent() extends BuiltinIdent {
  def prettyPrint = " /r "
}

case class TIntPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +i "
}

case class TIntMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -i "
}

case class TIntTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *i "
}

case class TIntDivIdent() extends BuiltinIdent {
  def prettyPrint = " /i "
}

case class TIntModIdent() extends BuiltinIdent {
  def prettyPrint = " mod "
}

case class TStringCatIdent() extends BuiltinIdent {
  def prettyPrint = " ^s "
}

case class TIntLEQIdent() extends BuiltinIdent {
  def prettyPrint = " =i "
}

case class TIntGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=i "
}

case class TIntLTIdent() extends BuiltinIdent {
  def prettyPrint = " <i "
}

case class TIntGTIdent() extends BuiltinIdent {
  def prettyPrint = " >i "
}

case class TRealLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=r "
}

case class TRealGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=r "
}

case class TRealLTIdent() extends BuiltinIdent {
  def prettyPrint = " <r "
}

case class TRealGTIdent() extends BuiltinIdent {
  def prettyPrint = " >r "
}

case class TStringLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=s "
}

case class TStringGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=s "
}

case class TStringLTIdent() extends BuiltinIdent {
  def prettyPrint = " <s "
}

case class TStringGTIdent() extends BuiltinIdent {
  def prettyPrint = " >s "
}

/* Although equals does not have to be specialized, we do so because
 * it can yeild performance gains if it is.
 */
case class TGenericEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =gen "
}

case class TIntEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =i "
}

case class TRealEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =r "
}

case class TStringEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =s "
}


case class TAnd() extends BuiltinIdent {
  def prettyPrint = " andalso "
}

case class TOr() extends BuiltinIdent {
  def prettyPrint = " orelse "
}

case class TNegInt() extends BuiltinIdent {
  def prettyPrint = " ~i "
}

case class TNegReal() extends BuiltinIdent {
  def prettyPrint = " ~r "
}

case class TNot() extends BuiltinIdent {
  def prettyPrint = " not "
}

case class TPrint() extends BuiltinIdent {
  def prettyPrint = " print "
}

package tir

import scala.collection.mutable.HashSet

sealed trait TIdent extends TTree {
  def nodeClone: TIdent

  def getDeclaredIdents: HashSet[TNamedIdent]
}

sealed trait BuiltinIdent extends TIdent {
  def getDeclaredIdents = new HashSet[TNamedIdent]()
}

sealed trait TNamedIdent extends TIdent {
  def nodeClone: TNamedIdent

  def getDeclaredIdents = {
    val set = new HashSet[TNamedIdent]()
    set += (this)
    set
  }
}

case class TIdentTuple(var subTypes: List[TIdent]) extends TIdent {
  def prettyPrint = "(" + subTypes.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone =
    new TIdentTuple(subTypes.map(_.nodeClone))

  def getDeclaredIdents =
    subTypes.map(_.getDeclaredIdents).foldLeft(new HashSet[TNamedIdent]) {
      case (buildup, next) => buildup union next
    }
}

case class TIdentVar(var name: String, var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = name

  def nodeClone =
    new TIdentVar(new String(name), identClass.nodeClone)
}

// This is a class for representing type tags on nodes.
case class TInternalIdentVar(var name: String) extends TNamedIdent {
  def prettyPrint = "Internal_" + name

  def nodeClone =
    new TInternalIdentVar(new String(name))
}

case class TIdentLongVar(var name: List[String], var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = name.mkString(".")

  def nodeClone =
    new TIdentLongVar(name.map(new String(_)), identClass.nodeClone)
}

/* This class is introduced in the LowerProgram pass.
 * The first argument is to make the node unique.  */
case class TArgumentNode(var funname: TTopLevelIdent,
                         var argNumber: Int) extends TNamedIdent {
  // Argument nodes may be assumed to be of TValClass
  def prettyPrint = "Argument_" + argNumber

  def nodeClone =
    new TArgumentNode(funname.nodeClone, argNumber)
}

case class TTopLevelIdent(var name: String, var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = "TopLevel_" + name

  def nodeClone =
    new TTopLevelIdent(new String(name), identClass.nodeClone)
}

case class TNumberedIdentVar(var name: String, var number: Int)
    extends TNamedIdent {
  // Numbered Ident Vars may assumed to be of TValClass
  def prettyPrint = name + "_" + number.toString

  def nodeClone =
    new TNumberedIdentVar(new String(name), number)
}

case class TUnderscoreIdent() extends BuiltinIdent {
  def prettyPrint = "_"

  def nodeClone = new TUnderscoreIdent()
}

case class TUnitIdent() extends BuiltinIdent {
  def prettyPrint = "()"

  def nodeClone = new TUnitIdent()
}

case class TConsIdent() extends BuiltinIdent {
  def prettyPrint = "::"

  def nodeClone = new TConsIdent()
}

case class TAppendIdent() extends BuiltinIdent {
  def prettyPrint = "@"

  def nodeClone = new TAppendIdent()
}

case class TEmptyListIdent() extends BuiltinIdent {
  def prettyPrint = "[]"

  def nodeClone = new TEmptyListIdent()
}

case class TRealPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +r "

  def nodeClone = new TRealPlusIdent()
}

case class TRealMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -r "

  def nodeClone = new TRealMinusIdent()
}

case class TRealTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *r "

  def nodeClone = new TRealTimesIdent()
}

case class TRealDivIdent() extends BuiltinIdent {
  def prettyPrint = " /r "

  def nodeClone = new TRealDivIdent()
}

case class TIntPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +i "

  def nodeClone = new TIntPlusIdent()
}

case class TIntMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -i "

  def nodeClone = new TIntMinusIdent()
}

case class TIntTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *i "

  def nodeClone = new TIntTimesIdent()
}

case class TIntDivIdent() extends BuiltinIdent {
  def prettyPrint = " /i "

  def nodeClone = new TIntDivIdent()
}

case class TIntModIdent() extends BuiltinIdent {
  def prettyPrint = " mod "

  def nodeClone = new TIntModIdent()
}

case class TStringCatIdent() extends BuiltinIdent {
  def prettyPrint = " ^s "

  def nodeClone = new TStringCatIdent()
}

case class TIntLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=i "

  def nodeClone = new TIntLEQIdent()
}

case class TIntGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=i "

  def nodeClone = new TIntGEQIdent()
}

case class TIntLTIdent() extends BuiltinIdent {
  def prettyPrint = " <i "

  def nodeClone = new TIntLTIdent()
}

case class TIntGTIdent() extends BuiltinIdent {
  def prettyPrint = " >i "

  def nodeClone = new TIntGTIdent()
}

case class TRealLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=r "

  def nodeClone = new TRealLEQIdent()
}

case class TRealGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=r "

  def nodeClone = new TRealGEQIdent()
}

case class TRealLTIdent() extends BuiltinIdent {
  def prettyPrint = " <r "

  def nodeClone = new TRealLTIdent()
}

case class TRealGTIdent() extends BuiltinIdent {
  def prettyPrint = " >r "

  def nodeClone = new TRealGTIdent()
}

case class TStringLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=s "

  def nodeClone = new TStringLEQIdent()
}

case class TStringGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=s "

  def nodeClone = new TStringGEQIdent()
}

case class TStringLTIdent() extends BuiltinIdent {
  def prettyPrint = " <s "

  def nodeClone = new TStringLTIdent()
}

case class TStringGTIdent() extends BuiltinIdent {
  def prettyPrint = " >s "

  def nodeClone = new TStringGTIdent()
}

/* Although equals does not have to be specialized, we do so because
 * it can yeild performance gains if it is.
 */
case class TGenericEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =gen "

  def nodeClone = new TGenericEqualsIdent()
}

case class TIntEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =i "

  def nodeClone = new TIntEqualsIdent()
}

case class TRealEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =r "

  def nodeClone = new TRealEqualsIdent()
}

case class TStringEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =s "

  def nodeClone = new TStringEqualsIdent()
}

case class TBoolEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =b "

  def nodeClone = new TBoolEqualsIdent()
}

case class TAnd() extends BuiltinIdent {
  def prettyPrint = " andalso "

  def nodeClone = new TAnd()
}

case class TOr() extends BuiltinIdent {
  def prettyPrint = " orelse "

  def nodeClone = new TOr()
}

case class TNegInt() extends BuiltinIdent {
  def prettyPrint = " ~i "

  def nodeClone = new TNegInt()
}

case class TNegReal() extends BuiltinIdent {
  def prettyPrint = " ~r "

  def nodeClone = new TNegReal()
}

case class TNot() extends BuiltinIdent {
  def prettyPrint = " not "

  def nodeClone = new TNot()
}

case class TPrint() extends BuiltinIdent {
  def prettyPrint = " print "

  def nodeClone = new TPrint()
}

/* This class has some special cases for built in exceptions.  */
trait TIdentThrowable extends TIdent {
  def nodeClone: TIdentThrowable

  def getDeclaredIdents = new HashSet[TNamedIdent]()
}

case class TIdentMatchError() extends TIdentThrowable {
  def prettyPrint = "raise MatchError"

  def nodeClone = new TIdentMatchError()
}

package tir

import scala.collection.mutable.HashSet
import typecheck.VariableGenerator

sealed trait TIdent extends TTree {
  def nodeClone(env: TTypeEnv): TIdent

  def getDeclaredIdents: HashSet[TNamedIdent]
}

sealed trait BuiltinIdent extends TIdent {
  def getDeclaredIdents = new HashSet[TNamedIdent]()
}

sealed trait TNamedIdent extends TIdent {
  def nodeClone(env: TTypeEnv): TNamedIdent

  def getDeclaredIdents = {
    val set = new HashSet[TNamedIdent]()
    set += (this)
    set
  }
}

case class TIdentTuple(var subTypes: List[TIdent]) extends TIdent {
  def prettyPrint = "(" + subTypes.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone(env: TTypeEnv) =
    new TIdentTuple(subTypes.map(_.nodeClone(env)))

  def getDeclaredIdents =
    subTypes.map(_.getDeclaredIdents).foldLeft(new HashSet[TNamedIdent]) {
      case (buildup, next) => buildup union next
    }
}

case class TIdentVar(var name: String, var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = name

  def nodeClone(env: TTypeEnv) =
    new TIdentVar(new String(name), identClass.nodeClone(env))
}

// This is a class for representing type tags on nodes.  Each
// TInternalIdentVar MUST be typed at the top level and MUST
// appear at most once.
case class TInternalIdentVar(var name: String) extends TNamedIdent {
  def prettyPrint = "Internal_" + name

  def nodeClone(env: TTypeEnv) = {
    val newNode = VariableGenerator.newTInternalVariable()
    env.addTopLevel(newNode, env.getOrFail(this), false)
    newNode
  }
}

case class TIdentLongVar(var name: List[String], var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = name.mkString(".")

  def nodeClone(env: TTypeEnv) =
    new TIdentLongVar(name.map(new String(_)), identClass.nodeClone(env))
}

/* This class is introduced in the LowerProgram pass.
 * The first argument is to make the node unique.  */
case class TArgumentNode(var funname: TTopLevelIdent,
                         var argNumber: Int) extends TNamedIdent {
  // Argument nodes may be assumed to be of TValClass
  def prettyPrint = "Argument_" + argNumber

  def nodeClone(env: TTypeEnv) =
    new TArgumentNode(funname.nodeClone(env), argNumber)
}

case class TTopLevelIdent(var name: String, var identClass: TIdentClass)
    extends TNamedIdent {
  def prettyPrint = "TopLevel_" + name

  def nodeClone(env: TTypeEnv) =
    new TTopLevelIdent(new String(name), identClass.nodeClone(env))
}

case class TNumberedIdentVar(var name: String, var number: Int)
    extends TNamedIdent {
  // Numbered Ident Vars may assumed to be of TValClass
  def prettyPrint = name + "_" + number.toString

  def nodeClone(env: TTypeEnv) =
    new TNumberedIdentVar(new String(name), number)
}

case class TUnderscoreIdent() extends BuiltinIdent {
  def prettyPrint = "_"

  def nodeClone(env: TTypeEnv) = new TUnderscoreIdent()
}

case class TUnitIdent() extends BuiltinIdent {
  def prettyPrint = "()"

  def nodeClone(env: TTypeEnv) = new TUnitIdent()
}

case class TConsIdent() extends BuiltinIdent {
  def prettyPrint = "::"

  def nodeClone(env: TTypeEnv) = new TConsIdent()
}

case class TAppendIdent() extends BuiltinIdent {
  def prettyPrint = "@"

  def nodeClone(env: TTypeEnv) = new TAppendIdent()
}

case class TEmptyListIdent() extends BuiltinIdent {
  def prettyPrint = "[]"

  def nodeClone(env: TTypeEnv) = new TEmptyListIdent()
}

case class TRealPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +r "

  def nodeClone(env: TTypeEnv) = new TRealPlusIdent()
}

case class TRealMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -r "

  def nodeClone(env: TTypeEnv) = new TRealMinusIdent()
}

case class TRealTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *r "

  def nodeClone(env: TTypeEnv) = new TRealTimesIdent()
}

case class TRealDivIdent() extends BuiltinIdent {
  def prettyPrint = " /r "

  def nodeClone(env: TTypeEnv) = new TRealDivIdent()
}

case class TIntPlusIdent() extends BuiltinIdent {
  def prettyPrint = " +i "

  def nodeClone(env: TTypeEnv) = new TIntPlusIdent()
}

case class TIntMinusIdent() extends BuiltinIdent {
  def prettyPrint = " -i "

  def nodeClone(env: TTypeEnv) = new TIntMinusIdent()
}

case class TIntTimesIdent() extends BuiltinIdent {
  def prettyPrint = " *i "

  def nodeClone(env: TTypeEnv) = new TIntTimesIdent()
}

case class TIntDivIdent() extends BuiltinIdent {
  def prettyPrint = " /i "

  def nodeClone(env: TTypeEnv) = new TIntDivIdent()
}

case class TIntModIdent() extends BuiltinIdent {
  def prettyPrint = " mod "

  def nodeClone(env: TTypeEnv) = new TIntModIdent()
}

case class TStringCatIdent() extends BuiltinIdent {
  def prettyPrint = " ^s "

  def nodeClone(env: TTypeEnv) = new TStringCatIdent()
}

case class TIntLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=i "

  def nodeClone(env: TTypeEnv) = new TIntLEQIdent()
}

case class TIntGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=i "

  def nodeClone(env: TTypeEnv) = new TIntGEQIdent()
}

case class TIntLTIdent() extends BuiltinIdent {
  def prettyPrint = " <i "

  def nodeClone(env: TTypeEnv) = new TIntLTIdent()
}

case class TIntGTIdent() extends BuiltinIdent {
  def prettyPrint = " >i "

  def nodeClone(env: TTypeEnv) = new TIntGTIdent()
}

case class TRealLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=r "

  def nodeClone(env: TTypeEnv) = new TRealLEQIdent()
}

case class TRealGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=r "

  def nodeClone(env: TTypeEnv) = new TRealGEQIdent()
}

case class TRealLTIdent() extends BuiltinIdent {
  def prettyPrint = " <r "

  def nodeClone(env: TTypeEnv) = new TRealLTIdent()
}

case class TRealGTIdent() extends BuiltinIdent {
  def prettyPrint = " >r "

  def nodeClone(env: TTypeEnv) = new TRealGTIdent()
}

case class TStringLEQIdent() extends BuiltinIdent {
  def prettyPrint = " <=s "

  def nodeClone(env: TTypeEnv) = new TStringLEQIdent()
}

case class TStringGEQIdent() extends BuiltinIdent {
  def prettyPrint = " >=s "

  def nodeClone(env: TTypeEnv) = new TStringGEQIdent()
}

case class TStringLTIdent() extends BuiltinIdent {
  def prettyPrint = " <s "

  def nodeClone(env: TTypeEnv) = new TStringLTIdent()
}

case class TStringGTIdent() extends BuiltinIdent {
  def prettyPrint = " >s "

  def nodeClone(env: TTypeEnv) = new TStringGTIdent()
}

/* Although equals does not have to be specialized, we do so because
 * it can yeild performance gains if it is.
 */
case class TGenericEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =gen "

  def nodeClone(env: TTypeEnv) = new TGenericEqualsIdent()
}

case class TIntEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =i "

  def nodeClone(env: TTypeEnv) = new TIntEqualsIdent()
}

case class TRealEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =r "

  def nodeClone(env: TTypeEnv) = new TRealEqualsIdent()
}

case class TStringEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =s "

  def nodeClone(env: TTypeEnv) = new TStringEqualsIdent()
}

case class TBoolEqualsIdent() extends BuiltinIdent {
  def prettyPrint = " =b "

  def nodeClone(env: TTypeEnv) = new TBoolEqualsIdent()
}

case class TAnd() extends BuiltinIdent {
  def prettyPrint = " andalso "

  def nodeClone(env: TTypeEnv) = new TAnd()
}

case class TOr() extends BuiltinIdent {
  def prettyPrint = " orelse "

  def nodeClone(env: TTypeEnv) = new TOr()
}

case class TNegInt() extends BuiltinIdent {
  def prettyPrint = " ~i "

  def nodeClone(env: TTypeEnv) = new TNegInt()
}

case class TNegReal() extends BuiltinIdent {
  def prettyPrint = " ~r "

  def nodeClone(env: TTypeEnv) = new TNegReal()
}

case class TNot() extends BuiltinIdent {
  def prettyPrint = " not "

  def nodeClone(env: TTypeEnv) = new TNot()
}

case class TPrint() extends BuiltinIdent {
  def prettyPrint = " print "

  def nodeClone(env: TTypeEnv) = new TPrint()
}

/* This class has some special cases for built in exceptions.  */
trait TIdentThrowable extends TIdent {
  def nodeClone(env: TTypeEnv): TIdentThrowable

  def getDeclaredIdents = new HashSet[TNamedIdent]()
}

case class TIdentMatchError() extends TIdentThrowable {
  def prettyPrint = "raise MatchError"

  def nodeClone(env: TTypeEnv) = new TIdentMatchError()
}

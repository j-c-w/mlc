package tir

import toplev.GenericPrintable
import toplev.GenericType
import toplev.TypeClassSet
import toplev.GenericUnifier
import tpass.TPass

/* Note that since the typechecking has complete, we  don not expect
 * any type sets to remain.
 */

sealed trait TType extends TWalkable with GenericPrintable
                                     with GenericType[TType] {
  // These are left unimplemented as they are not
  // required by this phase of the compiler.
  //
  // They should be implemented in future to make this
  // pass more versatile.
  def contains(otherType: TType): Boolean = ???
  def getTypeVars(): TypeClassSet[TType] = ???
  def specializesTo(otherType: TType): Boolean = ???
  def substituteFor(subFor: TType,subIn: TType): TType = ???
  def typeClone(types: toplev.TypeClassSet[TType]): TType = ???
  def typeClone(): TType = ???
  def unify(typ: TType): GenericUnifier[TType] = ???
}

case class TFunctionType(var argType: TType, var resType: TType)
    extends TType {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    argType.walk(item, f)
    resType.walk(item, f)
  }

  def prettyPrint = "(%s -> %s)".format(argType.prettyPrint, resType.prettyPrint)
}

case class TTupleType(var subTypes: List[TType]) extends TType {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    subTypes.foreach(_.walk(item, f))
  }

  def prettyPrint = "(" + subTypes.map(_.prettyPrint).mkString(", ") + ")"
}

case class TEqualityTypeVar(var name: String) extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "''" + name
}

case class TUnconstrainedTypeVar(var name: String) extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "'" + name
}

case class TListType(var subType: TType) extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = subType.prettyPrint + " list" 
}

case class TIntType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "int"
}

case class TStringType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "string"
}

case class TRealType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "real"
}

case class TBoolType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "bool"
}

case class TCharType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "char"
}

case class TUnitType() extends TType {
  def walk[T](item: T, f: TPass[T]) = f(item, this)

  def prettyPrint = "unit"
}

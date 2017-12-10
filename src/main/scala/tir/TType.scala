package tir

import exceptions.ICE
import scala.collection.mutable.Map
import toplev.GenericPrintable
import toplev.GenericType
import toplev.GenericTypeSet
import toplev.GenericUnifier
import tpass.TPass

/* Note that since the typechecking has complete, we  don not expect
 * any type sets to remain.
 */

sealed trait TType extends TTree with GenericType[TType] {
  // These are left unimplemented as they are not
  // required by this phase of the compiler.
  //
  // They should be implemented in future to make this
  // pass more versatile.
  def contains(otherType: TType): Boolean = ???
  def specializesTo(otherType: TType): Boolean = ???
  def substituteFor(subFor: TType,subIn: TType): TType = ???
  def unify(typ: TType): GenericUnifier[TType] = ???
  def nodeClone: TType = typeClone
}

case class TFunctionType(var argType: TType, var resType: TType)
    extends TType {
  def getTypeVars() =
    argType.getTypeVars union resType.getTypeVars

  def substituteFor(map: Map[TType, TType]): TType =
    new TFunctionType(argType.substituteFor(map), resType.substituteFor(map))

  def atomicClone = throw new ICE("Error: TFunctionType is not atmoic")

  def prettyPrint =
    "(%s -> %s)".format(argType.prettyPrint, resType.prettyPrint)
}

case class TTupleType(var subTypes: List[TType])
    extends TType with TFlattenable[TType] {
  def getTypeVars() = {
    val emptySet: GenericTypeSet[TType] = new TTypeSet()

    subTypes.map(_.getTypeVars).foldLeft(emptySet) {
      case (set, nextSet) => set union nextSet
    }
  }

  def substituteFor(map: Map[TType, TType]): TType =
    new TTupleType(subTypes.map(_.substituteFor(map)))

  def atomicClone = throw new ICE("Error: TTupleType is not atmoic")

  def prettyPrint = "(" + subTypes.map(_.prettyPrint).mkString(", ") + ")"

  def flatten = if (subTypes.length == 1)
    subTypes(0) match {
      case flattenable: TFlattenable[TType] @unchecked => flattenable.flatten
      case other => other
    }
  else
    this
}

case class TEqualityTypeVar(var name: String) extends TType {
  def getTypeVars() = {
    val set = new TTypeSet()
    set.insert(this)

    set
  }

  def substituteFor(map: Map[TType, TType]): TType =
    if (map.contains(this)) {
      map(this).atomicClone
    } else {
      // Create a new variable node to avoid confusion.
      new TEqualityTypeVar(name)
    }

  def atomicClone = new TEqualityTypeVar(name)

  def prettyPrint = "''" + name
}

case class TUnconstrainedTypeVar(var name: String) extends TType {
  def getTypeVars() = {
    val set = new TTypeSet()
    set.insert(this)

    set
  }

  def substituteFor(map: Map[TType, TType]): TType =
    if (map.contains(this)) {
      map(this).atomicClone
    } else {
      // Create a new variable node to avoid confusion.
      new TUnconstrainedTypeVar(name)
    }

  def atomicClone = new TUnconstrainedTypeVar(name)

  def prettyPrint = "'" + name
}

case class TListType(var subType: TType) extends TType {
  def getTypeVars() =
    subType.getTypeVars

  def substituteFor(map: Map[TType, TType]): TType =
    new TListType(subType.substituteFor(map))

  def atomicClone = throw new ICE("Error: TUnconstrainedTypeVar is not atmoic")

  def prettyPrint = subType.prettyPrint + " list" 
}

case class TIntType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TIntType is not atmoic")

  def prettyPrint = "int"
}

case class TStringType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TIntType is not atmoic")

  def prettyPrint = "string"
}

case class TRealType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TRealType is not atmoic")

  def prettyPrint = "real"
}

case class TBoolType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TBoolType is not atmoic")

  def prettyPrint = "bool"
}

case class TCharType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TCharType is not atmoic")

  def prettyPrint = "char"
}

case class TUnitType() extends TType {
  def getTypeVars() = new TTypeSet()

  def substituteFor(map: Map[TType, TType]): TType =
    this

  def atomicClone = throw new ICE("Error: TUnitType is not atmoic")

  def prettyPrint = "unit"
}

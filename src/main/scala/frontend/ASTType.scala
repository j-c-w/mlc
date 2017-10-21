package frontend

import exceptions._
import toplev.GenericPrintable
import toplev.GenericType
import typecheck.TypeVariableGenerator

object ASTType {
  def unify(t1: ASTType, t2: ASTType): ASTType = t1 unify t2

  def isValidSpecialization(from: ASTType, to: ASTType) = {
    // This is implemented in terms of the unify function. The logic
    // is as follows:
    //    if we can unify the two types, and the result is the same
    //    as the 'to', then from must specialize to to.
    ???
  }
}

sealed trait ASTType extends GenericPrintable with GenericType[ASTType] {
  /* This returns true if the other type is contained within this type.
   * This is used to prevent infinite types from coming about.
   *
   * Note that this is only currently implemented for atomic tyvars.
   */
  def contains(other: ASTType): Boolean

  /* This was originally in the companion object. However, due to the vast
   * number of special cases, this is no longer put there.
   */
  def unify(other: ASTType): ASTType
  
  /* Types are considered atomic if they cannot be broken down into
   * other types. E.g. functions are not atomic, but 'int' is.
   */
  val isAtomic: Boolean
}

case class ASTTypeFunction(val arg: ASTType,
                           val result: ASTType) extends ASTType {
  def prettyPrint = " %s -> %s ".format(arg, result)
  def contains(other: ASTType) =
    arg.contains(other) || result.contains(other)
  def unify(other: ASTType) = other match {
    case (ASTTypeFunction(a1, r1)) =>
      ASTTypeFunction((arg unify a1), (result unify r1))
    case (ASTUnconstrainedTypeVar(name)) =>
      this
    case _ => throw new UnificationError(this, other)
  }
  val isAtomic = false
}

// To avoid ambiguity, there must be at least two
// types in this type.
case class ASTTypeTuple(val args: List[ASTType]) extends ASTType {
  def prettyPrint = " " + args.mkString(" * ") + " "
  def contains(other: ASTType) =
    args.exists((x) => x.contains(other))
  def unify(other: ASTType) = other match {
    case (ASTTypeTuple(typeSeq)) => {
      if (args.length != typeSeq.length) {
        throw new UnificationError(ASTTypeTuple(args),
                                   ASTTypeTuple(typeSeq))
      } else {
        ASTTypeTuple(((args zip typeSeq) map {
            case (x: ASTType, y: ASTType) => x unify y
        }))
      }
    }
    case (ASTUnconstrainedTypeVar(name)) =>
      this
    case (ASTEqualityTypeVar(name)) => {
      // This is a hard case.
      // We approach it by generating a tuple of equality type
      // variables, and attempting to unify those two.
      val equalityTyps = TypeVariableGenerator.getEqualityVars(args.length)

      this.unify(ASTTypeTuple(equalityTyps))
    }
    case (_) => throw new UnificationError(this, other)
  }
  val isAtomic = false
}

sealed trait ASTTypeVar extends ASTType

case class ASTEqualityTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "''" + name

  def contains(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) => otherName == name
    case _ => false
  }

  /* Because deciding whether some type admits equality or not,
   * this function pushes the unification of that into most other
   * types.
   */
  def unify(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) =>
      this
    case other => other.unify(this)
  }
  val isAtomic = true
}

case class ASTUnconstrainedTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "'" + name
  def contains(other: ASTType) = other match {
    case ASTUnconstrainedTypeVar(otherName) => otherName == name
    case _ => false
  }
  def unify(other: ASTType) =
    if (other.contains(this))
      throw new UnificationError(this, other)
    else
      other
  val isAtomic = true
}

case class ASTListType(subType: ASTType) extends ASTTypeVar {
  def prettyPrint = subType.prettyPrint + " list"
  def contains(other: ASTType) = subType.contains(other)
  def unify(other: ASTType) = other match {
    case ASTListType(typ) => ASTListType(subType.unify(typ))
    case ASTEqualityTypeVar(name) => {
      ASTListType(subType.unify(TypeVariableGenerator.getEqualityVar()))
    }
    case tyVar @ ASTUnconstrainedTypeVar(name) =>
      if (this.contains(tyVar))
        throw new UnificationError(this, tyVar)
      else
        this
  }
  val isAtomic = false
}

/* This class is a special case. The only function type that ML offers
 * polymorphism for are the builtin arithmetic operators.
 *
 * For some types of expression, e.g. f x = (~x, Real.toString(x))
 *
 * We need to keep both type options open until the whole environment for
 * x has been typed.  Hence this is defined, with a default to 'int'
 * during final unification.
 */
case class ASTNumberType() extends ASTTypeVar {
  def prettyPrint = "{int, real}"
  def contains(other: ASTType) = false
  val isAtomic = true

  def unify(other: ASTType) = other match {
    case ASTNumberType() => this
    case ASTRealType() => other
    case ASTIntType() => other
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => ASTIntType()
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTIntType() extends ASTTypeVar {
  def prettyPrint = "int"
  def contains(other: ASTType) = false
  val isAtomic = true

  def unify(other: ASTType) = other match {
    case ASTNumberType() => this
    case ASTIntType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTRealType() extends ASTTypeVar {
  def prettyPrint = "real"
  def contains(other: ASTType) = false
  val isAtomic = true

  def unify(other: ASTType) = other match {
    case ASTNumberType() => this
    case ASTRealType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTBoolType() extends ASTTypeVar {
  def prettyPrint = "bool"
  def contains(other: ASTType) = false
  val isAtomic = true
  def unify(other: ASTType) = other match {
    case ASTBoolType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTStringType() extends ASTTypeVar {
  def prettyPrint = "string"
  def contains(other: ASTType) = false
  val isAtomic = true
  def unify(other: ASTType) = other match {
    case ASTStringType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTCharType() extends ASTTypeVar {
  def prettyPrint = "char"
  def contains(other: ASTType) = false
  val isAtomic = true
  def unify(other: ASTType) = other match {
    case ASTCharType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTDataTypeName(val name: String) extends ASTTypeVar {
  def prettyPrint = name
  def contains(other: ASTType) = false
  val isAtomic = true
  def unify(other: ASTType) = other match {
    case ASTDataTypeName(otherName) => 
      if (name == otherName)
        this
      else
        throw  new UnificationError(this, other)
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

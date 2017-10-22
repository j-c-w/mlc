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
    val resType = from unify to

    (resType equals to)
  }
}

sealed trait ASTType extends GenericPrintable with GenericType[ASTType] {
  /* This returns true if the other type is contained within this type.
   * This is used to prevent infinite types from coming about.
   */
  def contains(other: ASTType): Boolean

  /* This was originally in the companion object. However, due to the vast
   * number of special cases, this is no longer put there.
   */
  def unify(other: ASTType): ASTType

  // This specializes one type to another.
  def specializeTo(other: ASTType): ASTUnifier

  def equals(other: ASTType): Boolean =
    this.contains(other) && other.contains(this)

  // This typeClones the ASTType, creating new type variables for
  // each type variable leaf.
  def typeClone: ASTType

  /* Note that this is a little bit of a trick case. It may throw
   * exceptions! Consider if you call ASTNumberType.admitsEquality..
   * What should the result be?
   *
   * Yes, it definitely depends on what the purpose of your call is.
   *
   * Should you want a more general funciton, perhaps a call of
   * admitsEquality(overestimate: Bool) would do the trick.
   */
  def admitsEquality: Boolean

  /* Types are considered atomic if they cannot be broken down into
   * other types. E.g. functions are not atomic, but 'int' is.
   */
  val isAtomic: Boolean
}

case class ASTTypeFunction(val arg: ASTType,
                           val result: ASTType) extends ASTType {
  def prettyPrint = " %s -> %s ".format(arg.prettyPrint, result.prettyPrint)

  override def contains(other: ASTType) = other match {
    case ASTTypeFunction(arg1, res1) =>
      (arg equals arg1) && (result equals res1)
    case _ => arg.contains(other) || result.contains(other)
  }

  override def specializeTo(other: ASTType): ASTUnifier = other match {
    case ASTTypeFunction(otherArg, otherResult) => {
      val argUnifier = arg.specializeTo(otherArg)
      val resUnifier = result.specializeTo(otherResult)

      argUnifier.mgu(resUnifier)

      return argUnifier
    }
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case (ASTTypeFunction(a1, r1)) =>
      ASTTypeFunction((arg unify a1), (result unify r1))
    case (ASTUnconstrainedTypeVar(name)) =>
      this
    case _ => throw new UnificationError(this, other)
  }

  def typeClone = new ASTTypeFunction(arg.typeClone, result.typeClone)

  def admitsEquality = false

  val isAtomic = false
}

// To avoid ambiguity, there must be at least two
// types in this type.
case class ASTTypeTuple(val args: List[ASTType]) extends ASTType {
  def prettyPrint = " " + (args.map(_.prettyPrint)).mkString(" * ") + " "

  def contains(other: ASTType) = other match {
    case ASTTypeTuple(otherArgs) if otherArgs.length == args.length => 
      (otherArgs zip args).forall{
        case (x: ASTType, y: ASTType) => x equals y } ||
      args.exists((x) => x.contains(other))
    case _ => args.exists((x) => x.contains(other))
  }

  override def specializeTo(other: ASTType): ASTUnifier = other match {
    case ASTTypeTuple(otherArgs) => {
      if (args.length == otherArgs.length) {
        throw new SpecializationError(this, other)
      } else {
        val unifiers = (args zip otherArgs) map {
          case (x: ASTType, y: ASTType) => x specializeTo y
        }

        val mgu = ASTUnifier()

        unifiers foreach (mgu.mgu(_))
        return mgu
      }
    }
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
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

  def typeClone = new ASTTypeTuple(args.map(_.typeClone))

  def admitsEquality = throw new ICE(""" Tuple.admitsEquality may not
    be called as that is not nessecarily a well defined concept.""")

  val isAtomic = false
}

sealed trait ASTTypeVar extends ASTType

case class ASTEqualityTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "''" + name

  def contains(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) => otherName == name
    case _ => false
  }

  override def specializeTo(other: ASTType) = other match {
    case ASTTypeTuple(typs) => {
      // We cheat in this case. We create N new type equality vars
      // and attempt to specialize those. If that succeeeds, we throw
      // away the unifiers and just return this unifying to other
      val equalityTyps = TypeVariableGenerator.getEqualityVars(typs.length)

      (equalityTyps zip typs).map{ case(x, y) => x.specializeTo(y) }

      ASTUnifier(this, other)
    }
    case ASTNumberType(_) =>
      throw new SpecializationError(this, other)
    case ASTUnconstrainedTypeVar(_) =>
      throw new SpecializationError(this, other)
    case _ =>
      if (other.admitsEquality)
        ASTUnifier(this, other)
      else
        throw new SpecializationError(this, other)
  }

  /* Because deciding whether some type admits equality or not,
   * this function pushes the unification of that into most other
   * types.
   */
  override def unify(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) =>
      this
    case other => other.unify(this)
  }

  def typeClone = TypeVariableGenerator.getEqualityVar()

  def admitsEquality = true

  val isAtomic = true
}

case class ASTUnconstrainedTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "'" + name

  def contains(other: ASTType) = other match {
    case ASTUnconstrainedTypeVar(otherName) => otherName == name
    case _ => false
  }

  override def specializeTo(other: ASTType) = other match {
    case _ => ASTUnifier(this, other)
  }

  override def unify(other: ASTType) =
    if (other.contains(this))
      throw new UnificationError(this, other)
    else
      other

  def typeClone = TypeVariableGenerator.getVar()

  def admitsEquality = throw new ICE("""Cannot call ASTUnconstrainedTypeVar
    .admitsEquality, as that is not a well defined concept""")

  val isAtomic = true
}

case class ASTListType(subType: ASTType) extends ASTTypeVar {
  def prettyPrint = subType.prettyPrint + " list"

  override def contains(other: ASTType) = other match {
    case ASTListType(otherSubType) =>
      (otherSubType equals subType) || (subType contains otherSubType)
    case _ => subType.contains(other)
  }

  override def specializeTo(other: ASTType) = other match {
    case ASTListType(otherSubType) => subType specializeTo other
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTListType(typ) => ASTListType(subType.unify(typ))
    case ASTEqualityTypeVar(name) => {
      ASTListType(subType.unify(TypeVariableGenerator.getEqualityVar()))
    }
    case tyVar @ ASTUnconstrainedTypeVar(name) =>
      if (this.contains(tyVar))
        throw new UnificationError(this, tyVar)
      else
        this
    case _ => throw new UnificationError(this, other)
  }

  def typeClone = new ASTListType(subType.typeClone)
  
  def admitsEquality = false

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
case class ASTNumberType(id: String) extends ASTTypeVar {
  def prettyPrint = "(" + id + ": {int, real} )"

  def contains(other: ASTType) = other match {
    case ASTNumberType(otherID) => otherID == id
    case _ => false
  }

  def typeClone = TypeVariableGenerator.getNumberTypeVar()

  /*
   * This is a special case for this function. Clearly, it makes
   * little sense to ask whether this admits equality. This
   * must be handled as a special case in any caller.
   *
   * This is why this method is protected.
   */
  def admitsEquality = throw new ICE("""Cannot call
    ASTNumberType.admitsEquality as  that is not a well defined concept.""")

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTNumberType(otherID) => ASTUnifier(this, other)
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTNumberType(otherID) => this
    case ASTRealType() => other
    case ASTIntType() => other
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => ASTIntType()
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTIntType() extends ASTTypeVar {
  def prettyPrint = "int"

  def contains(other: ASTType) = other.isInstanceOf[ASTIntType]

  def typeClone = new ASTIntType()

  def admitsEquality = true

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTIntType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTNumberType(_) => this
    case ASTIntType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTRealType() extends ASTTypeVar {
  def prettyPrint = "real"

  def contains(other: ASTType) = other.isInstanceOf[ASTRealType]

  def typeClone = new ASTRealType()

  def admitsEquality = false

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTRealType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTNumberType(_) => this
    case ASTRealType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTBoolType() extends ASTTypeVar {
  def prettyPrint = "bool"

  def contains(other: ASTType) = other.isInstanceOf[ASTBoolType]

  def typeClone = new ASTBoolType()

  def admitsEquality = true

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTBoolType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTBoolType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTStringType() extends ASTTypeVar {
  def prettyPrint = "string"

  def contains(other: ASTType) = other.isInstanceOf[ASTStringType]

  def typeClone = new ASTStringType()

  def admitsEquality = true

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTStringType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTStringType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTCharType() extends ASTTypeVar {
  def prettyPrint = "char"

  def contains(other: ASTType) = other.isInstanceOf[ASTCharType]

  def typeClone = new ASTCharType()

  def admitsEquality = true

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case ASTCharType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def unify(other: ASTType) = other match {
    case ASTCharType() => this
    case ASTUnconstrainedTypeVar(name) => this
    case ASTEqualityTypeVar(name) => this
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTDataTypeName(val name: String) extends ASTTypeVar {
  def prettyPrint = name

  def contains(other: ASTType) = ???

  def typeClone = new ASTDataTypeName(name)

  def admitsEquality = ???

  val isAtomic = true

  override def specializeTo(other: ASTType) = other match {
    case _ => ???
  }

  override def unify(other: ASTType) = other match {
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

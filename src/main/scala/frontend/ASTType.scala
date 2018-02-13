package frontend

import exceptions._
import generators.TypeVariableGenerator
import toplev.GenericPrintable
import toplev.GenericType
import toplev.GenericTypeSet

import scala.collection.mutable.{HashMap,Map}

object ASTType {
  // This map contains whether datatypes have equality or not.  It is
  // set by the typecheck pass.  Cals to admitsEquality may fail before
  // that point.
  var admitsEqualityMap: Option[Map[ASTIdent, Boolean]] = None

  def unify(t1: ASTType, t2: ASTType): ASTUnifier = t1 unify t2

  def isValidSpecialization(from: ASTType, to: ASTType) = {
    from.specializesTo(to)
  }
}

sealed trait ASTType extends GenericPrintable with GenericType[ASTType] {
  /* This returns true if the other type is contained within this type.
   * This is used to prevent infinite types from coming about.
   */
  def contains(other: ASTType): Boolean = {
    if (other.isAtomic)
      this.containsAtomic(other)
    else
      this.containsNonAtomic(other)
  }

  /* This should be called when the other type is not atomic.
   * It is much slower.
   */
  def containsNonAtomic(other: ASTType): Boolean

  /* This returns true if one type can be directly represented by another
   * type with no unification.
   *
   * This arises in datatype definitions, for example:
   * DataConstructor(exn) should be the same as the exception type without
   * any unification.
   *
   * Note: For ease of implementation, this need not be a symmetric relation.
   */
  def is(other: ASTType): Boolean

  /* This returns true if the other type is contained within this type.
   *
   * This ONLY WORKS if the type 'other' is atomic
   */
  def containsAtomic(other: ASTType): Boolean

  /* This returns a set of all the type variables contained
   * within this type.
   */
  def getTypeVars(): GenericTypeSet[ASTType]

  /* This was originally in the companion object. However, due to the vast
   * number of special cases, this is no longer put there.
   */
  override def unify(other: ASTType): ASTUnifier = {
    if (this.contains(other) || other.contains(this)) {
      if ((other is this) || (this is other))
        ASTUnifier()
      else
        throw new UnificationError(this, other)
    } else {
      mguNoCyclicCheck(other)
    }
  }

  /* This finds MGUs. However, id does not do the cyclic check.
   * For that, the unify function should be called.
   */
  def mguNoCyclicCheck(other: ASTType): ASTUnifier

  // This specializes one type to another.
  def specializeTo(other: ASTType): ASTUnifier

  def specializesTo(other: ASTType): Boolean = {
    specializeTo(other)
    true
  }

  def equals(other: ASTType): Boolean =
    this.contains(other) && other.contains(this)

  /* If the type that this is called on is a clonable atomic type
   * (i.e. a tyvar), then a new tyvar of the same type but with
   * a different name is returned. Otherwise this raises an ICE.
   */
  def atomicClone: ASTType

  /* This takes all instances of 'subFor' in this type and
   * replaces them with 'subIn'.
   */
  def substituteFor(subFor: ASTType, subIn: ASTType): ASTType = {
    val map = new HashMap[ASTType, ASTType]()
    map(subFor) = subIn
    substituteFor(map)
  }

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

  /* This returns true if the type is monomorphic and false if it is
   * polymorphic.  */
  def isMonomorphic: Boolean
}

case class ASTFunctionType(val arg: ASTType,
                           val result: ASTType) extends ASTType {
  def prettyPrint = " (%s -> %s) ".format(arg.prettyPrint, result.prettyPrint)

  override def containsNonAtomic(other: ASTType) = other match {
    case ASTFunctionType(arg1, res1) =>
      (arg equals arg1) && (result equals res1)
    case _ => arg.contains(other) || result.contains(other)
  }

  override def is(other: ASTType) = other match {
    case ASTFunctionType(otherArg, otherRes) =>
      (arg is otherArg) && (result is otherRes)
    case _ => false
  }

  override def containsAtomic(other: ASTType) =
    arg.containsAtomic(other) || result.containsAtomic(other)

  override def getTypeVars() =
    (arg.getTypeVars()) union (result.getTypeVars())

  override def specializeTo(other: ASTType): ASTUnifier = other match {
    case ASTFunctionType(otherArg, otherResult) => {
      val argUnifier = arg.specializeTo(otherArg)
      val resUnifier = result.specializeTo(otherResult)

      argUnifier.mguUnify(resUnifier)

      return argUnifier
    }
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType): ASTUnifier = other match {
    case ASTFunctionType(otherArg, otherResult) => {
      val argUnifier = arg.unify(otherArg)
      val resUnifier = argUnifier(result).unify(argUnifier(otherResult))

      argUnifier mguUnify resUnifier

      argUnifier
    }
    case (ASTUnconstrainedTypeVar(name)) =>
      if (this.containsAtomic(other))
        throw new UnificationError(this, other)
      else
        ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      new ASTFunctionType(arg.substituteFor(map),
                          result.substituteFor(map))

  def admitsEquality = false

  val isAtomic = false

  def isMonomorphic = arg.isMonomorphic && result.isMonomorphic
}

// To avoid ambiguity, there must be at least two
// types in this type.
case class ASTTupleType(val args: List[ASTType]) extends ASTType {
  assert(args.length > 1)
  def prettyPrint = " (" + (args.map(_.prettyPrint)).mkString(" * ") + ") "

  override def containsNonAtomic(other: ASTType) = other match {
    case ASTTupleType(otherArgs) if otherArgs.length == args.length =>
      (otherArgs zip args).forall{
        case (x: ASTType, y: ASTType) => x equals y } ||
      args.exists((x) => x.contains(other))
    case _ => args.exists((x) => x.contains(other))
  }

  override def is(other: ASTType) = other match {
    case ASTTupleType(otherArgs) =>
      (args zip otherArgs).forall {
        case (arg, otherArg) => arg is otherArg
      }
    case _ => false
  }

  override def containsAtomic(other: ASTType) =
    args.exists(_.containsAtomic(other))

  override def getTypeVars() =
    args.foldRight (ASTTypeSet(): GenericTypeSet[ASTType]) {
        case (typ, set) => set union (typ.getTypeVars()) }

  override def specializeTo(other: ASTType): ASTUnifier = other match {
    case ASTTupleType(otherArgs) => {
      if (args.length != otherArgs.length) {
        throw new SpecializationError(this, other)
      } else {
        val unifiers = (args zip otherArgs) map {
          case (x: ASTType, y: ASTType) => x specializeTo y
        }

        val mgu = ASTUnifier()

        unifiers foreach (mgu.mguUnify(_))
        return mgu
      }
    }
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType): ASTUnifier = other match {
    case (ASTTupleType(typeSeq)) => {
      if (args.length != typeSeq.length)
        throw new UnificationError(this, other)
      else {
        val mgu = ASTUnifier()

        (args zip typeSeq) foreach {
          case (arg, otherArg) => {
            val intermediateUnifier = mgu(arg).unify(mgu(otherArg))
            mgu.mguUnify(intermediateUnifier)
          }
        }

        mgu
      }
    }
    case ASTUnconstrainedTypeVar(name) => {
      ASTUnifier(other, this)
    }
    case ASTEqualityTypeVar(name) => {
      // This is a hard case again.
      val equalityTyps = TypeVariableGenerator.getEqualityVars(args.length)

      val unifier = this.unify(ASTTupleType(equalityTyps))

      // If that did not crash, we still need that MGU as it might
      // require some specializations from within the tuple
      // It is OK to have all the superfluous mappings as the variables
      // they map from won't be used again.
      unifier.specializeNV(other, this)
      unifier
    }
    case _ => throw new UnificationError(this, other)
  }

  def substituteFor(map: Map [ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      new ASTTupleType(args.map(x => x.substituteFor(map)))

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = args.forall(_.admitsEquality)

  val isAtomic = false

  def isMonomorphic = args.forall(_.isMonomorphic)
}

sealed trait ASTTypeVar extends ASTType

case class ASTEqualityTypeVar(name: String) extends ASTTypeVar {
  def prettyPrint = "''" + name

  override def containsNonAtomic(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) => otherName == name
    case _ => false
  }

  override def is(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) =>
      otherName == name
    case _ => false
  }

  override def containsAtomic(other: ASTType) = containsNonAtomic(other)

  override def getTypeVars() =
    ASTTypeSet(this)

  override def specializeTo(other: ASTType) = other match {
    case ASTTupleType(typs) => {
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

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTEqualityTypeVar(otherName) =>
      if (this.name == otherName)
        ASTUnifier()
      else
        ASTUnifier(this, other)
    case _ => other.unify(this)
  }

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      this

  def atomicClone = TypeVariableGenerator.getEqualityVar()

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = false
}

case class ASTUnconstrainedTypeVar(name: String) extends ASTTypeVar {
  override def prettyPrint = "'" + name

  override def containsNonAtomic(other: ASTType) = other match {
    case ASTUnconstrainedTypeVar(otherName) => otherName == name
    case _ => false
  }

  override def is(other: ASTType) = other match {
    case ASTUnconstrainedTypeVar(otherName) =>
      otherName == name
    case _ => false
  }

  override def containsAtomic(other: ASTType) = containsNonAtomic(other)

  override def getTypeVars() =
    ASTTypeSet(this)

  override def specializeTo(other: ASTType) = other match {
    case _ => ASTUnifier(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTUnconstrainedTypeVar(otherName) =>
      if (otherName == this.name)
        ASTUnifier()
      else
        ASTUnifier(this, other)
    case _ => ASTUnifier(this, other)
  }

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      this

  def atomicClone = TypeVariableGenerator.getVar()

  def admitsEquality = throw new ICE("""Cannot call ASTUnconstrainedTypeVar
    .admitsEquality, as that is not a well defined concept""")

  val isAtomic = true

  def isMonomorphic = false
}

case class ASTListType(subType: ASTType) extends ASTTypeVar {
  def prettyPrint = subType.prettyPrint + " list"

  override def containsNonAtomic(other: ASTType) = other match {
    case ASTListType(otherSubType) =>
      (otherSubType equals subType) || (subType contains otherSubType)
    case _ => subType.contains(other)
  }

  override def is(other: ASTType) = other match {
    case ASTListType(otherSubType) =>
      subType is otherSubType
    case _ => false
  }

  override def containsAtomic(other: ASTType) =
    subType.containsAtomic(other)

  override def getTypeVars() =
    subType.getTypeVars()

  override def specializeTo(other: ASTType) = other match {
    case ASTListType(otherSubType) => subType specializeTo otherSubType
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTListType(typ) => {
      subType unify typ
    }
    case ASTEqualityTypeVar(name) => {
      val typVar = TypeVariableGenerator.getEqualityVar()

      val mgu = subType.unify(typVar)
      mgu.specializeNV(other, this)
      mgu
    }
    case ASTUnconstrainedTypeVar(name) =>
      ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      ASTListType(subType.substituteFor(map))

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = subType.admitsEquality

  val isAtomic = false

  def isMonomorphic = subType.isMonomorphic
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

  def containsNonAtomic(other: ASTType) = other match {
    case ASTNumberType(otherID) => otherID == id
    case _ => false
  }

  override def is(other: ASTType) = other match {
    case ASTNumberType(otherID) =>
      id == otherID
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      this

  def atomicClone = TypeVariableGenerator.getNumberTypeVar()

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

  def isMonomorphic = false

  override def getTypeVars() =
    ASTTypeSet(this)

  override def specializeTo(other: ASTType) = other match {
    case ASTNumberType(otherID) => ASTUnifier(this, other)
    case ASTRealType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTNumberType(otherID) =>
      if (id == otherID)
        ASTUnifier()
      else
        ASTUnifier(other, this)
    case ASTRealType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTIntStringCharType(id) => {
      val unifier = ASTUnifier()

      unifier.specializeNV(other, ASTIntType())
      unifier.specializeNV(this, ASTIntType())
      unifier
    }
    case ASTComparableType(id) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => {
      val unifier = ASTUnifier()

      unifier.specializeNV(other, ASTIntType())
      unifier.specializeNV(this, ASTIntType())
      unifier
    }
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTComparableType(val id: String) extends ASTTypeVar {
  def prettyPrint = "( " + id + ": { char, real, int, string })"

  def containsNonAtomic(other: ASTType) = other match {
    case ASTComparableType(otherID) => otherID == id
    case _ => false
  }

  override def is(other: ASTType) = other match {
    case ASTComparableType(otherID) =>
      id == otherID
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      this

  def atomicClone = TypeVariableGenerator.getComparableTypeVar()

  def admitsEquality = throw new ICE("""Cannot call
    ASTComparableType.admitsEquality as that is not a well defined concept.""")

  val isAtomic = true

  def isMonomorphic = false

  def getTypeVars() = ASTTypeSet(this)

  override def specializeTo(other: ASTType) = other match {
    case ASTComparableType(otherID) => ASTUnifier(this, other)
    case ASTCharType() => ASTUnifier(this, other)
    case ASTRealType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case ASTStringType() => ASTUnifier(this, other)
    case ASTIntStringCharType(id) => ASTUnifier(this, other)
    case ASTNumberType(id) => ASTUnifier(this, other)
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTComparableType(otherID) =>
      if (id == otherID)
        ASTUnifier()
      else
        ASTUnifier(other, this)
    case ASTCharType() => ASTUnifier(this, other)
    case ASTRealType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case ASTStringType() => ASTUnifier(this, other)
    case ASTIntStringCharType(id) => ASTUnifier(this, other)
    case ASTNumberType(id) => ASTUnifier(this, other)
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => {
      val unifier = ASTUnifier()
      val tyVar = TypeVariableGenerator.getIntStringCharTypeVar()

      unifier.specializeNV(other, tyVar)
      unifier.specializeNV(this, tyVar)

      unifier
    }
    case _ => throw new UnificationError(this, other)
  }
}

/* This is a type that should never be directly generated.
 * It is the result of unifying something we used in a comparison
 * with an equality type.
 */
case class ASTIntStringCharType(val id: String) extends ASTTypeVar {
  def prettyPrint = "( " + id + ": { int, string, char })"

  def containsNonAtomic(other: ASTType) = other match {
    case ASTIntStringCharType(otherID) => otherID == id
    case _ => false
  }

  override def is(other: ASTType) = other match {
    case ASTIntStringCharType(otherID) =>
      id == otherID
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    if (map.contains(this))
      map(this)
    else
      this

  def atomicClone = TypeVariableGenerator.getIntStringCharTypeVar()

  def admitsEquality = throw new ICE("""Cannot call
    ASTComparableType.admitsEquality as that is not a well defined concept.""")

  val isAtomic = true

  def isMonomorphic = false

  def getTypeVars() = ASTTypeSet(this)

  override def specializeTo(other: ASTType) = other match {
    case ASTIntStringCharType(otherID) => ASTUnifier(this, other)
    case ASTCharType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case ASTStringType() => ASTUnifier(this, other)
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTIntStringCharType(otherID) =>
      if (id == otherID)
        ASTUnifier()
      else
        ASTUnifier(other, this)
    case ASTCharType() => ASTUnifier(this, other)
    case ASTIntType() => ASTUnifier(this, other)
    case ASTStringType() => ASTUnifier(this, other)
    case ASTComparableType(id) => ASTUnifier(other, this)
    case ASTNumberType(id) => {
      // In this case, we must specialize both to int types.
      val unifier = ASTUnifier()

      unifier.specializeNV(this, ASTIntType())
      unifier.specializeNV(other, ASTIntType())

      unifier
    }
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTIntType() extends ASTTypeVar {
  def prettyPrint = "int"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTIntType]

  override def is(other: ASTType) = other match {
    case ASTIntType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTIntType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTNumberType(_) => ASTUnifier(other, this)
    case ASTComparableType(_) => ASTUnifier(other, this)
    case ASTIntStringCharType(_) => ASTUnifier(other, this)
    case ASTIntType() => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTRealType() extends ASTTypeVar {
  def prettyPrint = "real"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTRealType]

  override def is(other: ASTType) = other match {
    case ASTRealType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = false

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTRealType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTNumberType(_) => ASTUnifier(other, this)
    case ASTComparableType(_) => ASTUnifier(other, this)
    case ASTRealType() => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTBoolType() extends ASTTypeVar {
  def prettyPrint = "bool"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTBoolType]

  override def is(other: ASTType) = other match {
    case ASTBoolType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTBoolType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTBoolType() => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTStringType() extends ASTTypeVar {
  def prettyPrint = "string"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTStringType]

  override def is(other: ASTType) = other match {
    case ASTStringType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTStringType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTComparableType(_) => ASTUnifier(other, this)
    case ASTIntStringCharType(_) => ASTUnifier(other, this)
    case ASTStringType() => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTCharType() extends ASTTypeVar {
  def prettyPrint = "char"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTCharType]

  override def is(other: ASTType) = other match {
    case ASTCharType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTCharType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTCharType() => ASTUnifier()
    case ASTComparableType(_) => ASTUnifier(other, this)
    case ASTIntStringCharType(_) => ASTUnifier(other, this)
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTExceptionType() extends ASTTypeVar {
  def prettyPrint = "exn"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTExceptionType]

  override def is(other: ASTType) = other match {
    case ASTExceptionType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) = this

  def atomicClone = throw new ICE("""Attempted clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = false

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTExceptionType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTExceptionType() => ASTUnifier()
    // We only match constructor types with no arguments.
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTUnitType() extends ASTTypeVar {
  def prettyPrint = "unit"

  def containsNonAtomic(other: ASTType) = other.isInstanceOf[ASTUnitType]

  override def is(other: ASTType) = other match {
    case ASTUnitType() => true
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) = this

  def atomicClone = throw new ICE("""Attempted type clone of
    uncloneable type %s""".format(this.prettyPrint))

  def admitsEquality = true

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTUnitType() => ASTUnifier()
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTUnitType() => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case _ => throw new UnificationError(this, other)
  }
}

case class ASTDataType(val name: ASTIdentVar) extends ASTType {
  def prettyPrint =
    "datatype " + name

  def containsNonAtomic(other: ASTType) = other match {
    // Two constructor types are equal if they have the same name and all
    // the same types.
    case ASTDataType(otherName)
        if name == otherName => true
    case _ =>
      false
  }

  override def is(other: ASTType) = other match {
    case ASTDataType(otherName) => otherName == name
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    // As there may be no type variables in the argument types,
    // we can stop this here.
    this

  def atomicClone = throw new ICE("ASTDataType cannot be cloned")

  val isAtomic = false

  override def isMonomorphic = true

  override def admitsEquality = ASTType.admitsEqualityMap match {
    case Some(map) => map(name)
    case None =>
      throw new ICE("""Must set the admitsEqualityMap before calling
        |admitsEquality""".stripMargin)
  }

  // There can be no type variables in the arguments.
  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTDataType(otherName)
        if name == otherName => ASTUnifier()
    case ASTDataTypeName(otherName) if name == otherName =>
      ASTUnifier(this, other)
    case _ =>
      throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTDataType(otherName)
        if name == otherName => ASTUnifier()
    case ASTUnconstrainedTypeVar(name) =>
      ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) if admitsEquality =>
      ASTUnifier(other, this)
    case ASTDataTypeName(otherName) if name == otherName =>
      ASTUnifier(other, this)
    case other =>
      throw new SpecializationError(other, this)
  }
}

case class ASTDataTypeName(val name: ASTIdent) extends ASTTypeVar {
  def prettyPrint = name.prettyPrint

  def containsNonAtomic(other: ASTType) = other match {
    case ASTDataTypeName(otherName) => otherName == name
    case _ => false
  }

  def is(other: ASTType) = other match {
    case ASTDataTypeName(otherName) =>
      otherName == name
    case _ => false
  }

  def containsAtomic(other: ASTType) = containsNonAtomic(other)

  def substituteFor(map: Map[ASTType, ASTType]) =
    this

  def atomicClone = throw new ICE("ASTDataTypeName cannot be cloned")

  override def admitsEquality = ASTType.admitsEqualityMap match {
    case Some(map) => map(name)
    case None =>
      throw new ICE("""Must set the admitsEqualityMap before calling
        |admitsEquality""".stripMargin)
  }

  val isAtomic = true

  def isMonomorphic = true

  override def getTypeVars() = ASTTypeSet()

  override def specializeTo(other: ASTType) = other match {
    case ASTDataTypeName(otherName) =>
      if (name == otherName)
        ASTUnifier()
      else
        throw new SpecializationError(this, other)
    case ASTDataType(otherName) =>
      if (name == otherName)
        ASTUnifier()
      else
        throw new SpecializationError(this, other)
    case _ => throw new SpecializationError(this, other)
  }

  override def mguNoCyclicCheck(other: ASTType) = other match {
    case ASTDataTypeName(otherName) =>
      if (name == otherName)
        ASTUnifier()
      else
        throw  new UnificationError(this, other)
    case ASTUnconstrainedTypeVar(name) => ASTUnifier(other, this)
    case ASTEqualityTypeVar(name) => ASTUnifier(other, this)
    case ASTDataType(otherName) =>
      if (name == otherName)
        ASTUnifier()
      else
        throw  new UnificationError(this, other)
    case _ => throw new UnificationError(this, other)
  }
}

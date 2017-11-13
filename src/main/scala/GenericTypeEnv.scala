package toplev

import scala.collection.mutable.{Map,HashMap,HashSet}

import exceptions._
/* This is a generic type environment. It provides
 *
 * Note that  the map is inherently mutable.
 *
 * The parent stores the parent type environment,
 * to allow for variable name overloading. Note
 * that since typechecking has already occured, we
 * can assume that all variables identifiers within
 * a nesting level are unique.
 */

abstract class GenericTypeEnv[TypeEnvClass,
                              From <: GenericPrintable,
                              To <: GenericPrintable with GenericType[To]]
               (val parent: Option[GenericTypeEnv[TypeEnvClass, From, To]]) {
  def this() = this(None)

  // This check is to ensure correct use of the GenericTypeEnv class.
  assert(this.isInstanceOf[TypeEnvClass])

  /* Every type in this map can be used either as a type quantified
   * here or a type quantified elsewhere.
   *
   * Types that are quantified at this level of the type environment
   * are returned by typeCloning all of the subtypes in that type.
   */
  private val map: Map[From, (To, Option[GenericTypeSet[To]])] =
    HashMap[From, (To, Option[GenericTypeSet[To]])]()

  def prettyPrint = """

  %s

  """.format(map.map(pair => pair._1.prettyPrint + ": " +
                     pair._2._1.prettyPrint).mkString("\n  "))

  def hasType(id: From): Boolean =
    map.contains(id) || parent.map(_.hasType(id)).getOrElse(false)

  /* Searchs parents up unil (but not including) the passed
   * bound.  If the type if found, return true.  Otherwise, return
   * false.  */
  def hasTypeBetweenExclusive(bound: TypeEnvClass, id: From): Boolean =
    if (bound == this) {
      false
    } else {
      innermostHasType(id) || (parent match {
          case Some(parentEnv) => parentEnv.hasTypeBetweenExclusive(bound, id)
          case None =>
            throw new ICE("""Error: Parent type env did not appear in the
              |hierarchy""".stripMargin)
        })
    }

    def hasTypeBetweenInclusive(bound: TypeEnvClass, id: From):  Boolean =
      if (bound == this) {
        innermostHasType(id)
      } else {
        innermostHasType(id) || (parent match {
          case Some(parentEnv) => parentEnv.hasTypeBetweenInclusive(bound, id)
          case None =>
            throw new ICE("""Error: Parent type env did not appear in the
              |hierarchy""".stripMargin)
        })
      }

  // This call is always safe by the assertion made in the constructor.
  def getSelf: TypeEnvClass = this.asInstanceOf[TypeEnvClass]

  /* This seaches only this environment for the type. */
  def innermostHasType(id: From): Boolean =
    map.contains(id)

  def add(id: From, typ: To, qualified: Boolean): Unit =
    if (qualified)
      add(id, typ, Some(typ.getTypeVars()))
    else
      add(id, typ, None)

  def add(id: From, typ: To,
          qualifiedTypes: Option[GenericTypeSet[To]]): Unit = {
    map(id) = (typ, qualifiedTypes)
  }

  /* addTopLevel is very much like 'add', but it adds the mapping to this
   * map iff it has no parent. If it has a parent, the mapping is defered
   * to the parent (and so on up the tree).
   */
  def addTopLevel(id: From, typ: To, qualified: Boolean): Unit =
    if (qualified)
      addTopLevel(id, typ, Some(typ.getTypeVars()))
    else
      addTopLevel(id, typ, None)

  def addTopLevel(id: From, typ: To,
                  qualifiedTypes: Option[GenericTypeSet[To]]): Unit = {
    parent match {
      case Some(parentEnv) => parentEnv.addTopLevel(id, typ, qualifiedTypes)
      case None => add(id, typ, qualifiedTypes)
    }
  }

  /* The default qualified types is all the types or none of the
   * types
   */
  def updateId(id: From, newTyp: To, qualified: Boolean): Unit =
    if (qualified)
      updateId(id, newTyp, Some(newTyp.getTypeVars()))
    else
      updateId(id, newTyp, None)

  /* This function automatically validates that the variable it is
   * replacing is OK to replace with the one replacing it.
   *
   * Throw a runtime exception if that is not OK.
   *
   * This should be used unless you are really sure that it is OK.
   */
  def updateId(id: From, newTyp: To,
               quantifiedTypes: Option[GenericTypeSet[To]]): Unit = {
    getOrFail(id).unify(newTyp)
    updateIdNoValidate(id, newTyp, quantifiedTypes)
  }

  /* This attempts to update types in the parent if possible.  */
  def updateIdNoValidate(id: From, newTyp: To,
                         qualifiedTypes: Option[GenericTypeSet[To]]): Unit = {
    if (map.contains(id))
      map(id) = (newTyp, qualifiedTypes)
    else
      parent match {
        case Some(parentEnv) =>
          parentEnv.updateIdNoValidate(id, newTyp, qualifiedTypes)
        case None => {
          throw new ICE(""" Error, type %s not found in the map""".format(
            id.prettyPrint))
        }
      }
  }

  /* Given some name X (in the map) and some name Y (not in the map),
   * change the name of X to Y.
   *
   * This subsitution is done in the appropriate parent map.
   */
  def swapNames(from: From, to: From): Unit = {
    assert(hasType(from))
    assert(!hasType(to))

    if (map.contains(from)) {
      map(to) = map(from)
      map.remove(from)
    } else {
      // If this does not contain from, then we know the
      // parent must as we have already asserted that
      // the map has the right type.
      parent.get.swapNames(from, to)
    }
  }

  /* Given some identifier X, remove X from the map.
   * If it is in the parent, remove it from there (recursively).
   *
   * Throws if x is not in the map.  */
  def remove(x: From): Unit = {
    if (innermostHasType(x)) {
      map.remove(x)
    } else {
      parent match {
        case Some(parentEnv) => parentEnv.remove(x)
        case None => throw new ICE("""Type %s not found,
          |so could not be removed""".stripMargin.format(x.prettyPrint))
      }
    }
  }

  /* This gets a value from the map and substitutes
   * any quantified variables in for new variables.
   */
  def get(id: From): Option[To] = {
    val mapContents = map.get(id)

    mapContents match {
      case Some((typ, Some(qualifiedTypes))) =>
        Some(typ.typeClone(qualifiedTypes))
      case Some((typ, None)) => Some(typ)
      case None => // Try the parent
        parent match {
          case Some(parentEnv) => parentEnv.get(id)
          case None => None
        }
    }
  }

  def getOrFail(id: From): To = {
    get(id).getOrElse(throw new ICE(""" Error, type %s not found in
      |the environment""".stripMargin.format(id.prettyPrint)))
  }

  /* This returns all the unquantified types for some variable
   * in the map.
   *
   * This is used for unification to unify only unquantified types.
   */
  def getUnquantifiedTypesFor(id: From): GenericTypeSet[To] = {
    val (typ, quantified) = map(id)

    quantified match {
      case None => typ.getTypeVars()
      case Some(quantified) => typ.getTypeVars() - quantified
    }
  }

  /* This goes through all the atomics (INCLUDING QUANTIFIED ATOMICS)
   * that are used. It is used to change ASTNumberType -> ASTIntType
   * at the top level.
   *
   * It subsequently removes any forall quantified items
   * from the quantified section that have been replaced.
   */
  def specializeAtomicsMatching(f: (To => Boolean), sub: To): Unit = {
    // This check is made not beause this method cannot support
    // a sub with nested tyvars, but because it is significantly
    // more complicated. There are subtle failure modes with the lowering
    // pass if the quantifiedTypeVars aren't cleaned up.
    if (!sub.getTypeVars().isEmpty) {
      throw new ICE("""Cannot specialzeAtomics to poly types. Polytype used
        | was %s. """.stripMargin.format(sub.prettyPrint))
    }

    // If this is too slow, we could adjust the function definition
    // to only do the substituion once for any particular mapping.
    // Then keep track of the mappings and only do the new mappings.
    foreachInnermost({
      case(name, (to, quantifiedTypes)) => {
        val toVars = to.getTypeVars()
        var substitutedTo = to

        for (toVar <- toVars) {
          if (f(toVar)) {
            substitutedTo = substitutedTo.substituteFor(toVar, sub)
          }
        }

        // Despite the rather general definition of this method,
        // the target is NumType => ASTIntType (and the similar
        // group operations)
        val newTyVars = substitutedTo.getTypeVars()
        updateIdNoValidate(name, substitutedTo,
                           quantifiedTypes.map(_.intersection(newTyVars)))
      }
    })
  }

  /* This gets a value from the map and substitutes
   * any quantified variables in for new variables.
   */
  def apply(id: From): Option[To] = get(id)

  /* This returns the value from the map without
   * substituting in for the quantified types.
   */
  def getNoSubsitute(id: From): Option[To] =
    if (map.contains(id))
      map.get(id).map(_._1)
    else
      parent match {
        case Some(parentEnv) => parentEnv.getNoSubsitute(id)
        case None => None
      }

  def getNoSubsituteOrFail(id: From): To =
    getNoSubsitute(id).getOrElse(throw new ICE("""Error: Type %s
      |does not appear to be part of the environment""".stripMargin.format(
        id.prettyPrint)))

  /* This iterates over all elements in the environment and it's parents. */
  def foreachAll(f : (((From, (To, Option[GenericTypeSet[To]])))
      => Unit)): Unit = {
    map.foreach(f)
    parent.map(_.foreachAll(f))
  }

  /* This iterates over all elements in this environment (i.e. NOT
   * the parent environments). */
  def foreachInnermost(f: (((From, (To, Option[GenericTypeSet[To]])))
      => Unit)): Unit = {
    map.foreach(f)
  }

  /* This is used for unification.  It only iterates over elements that can
   * be 'seen' from this environment.  So, if 'x' is shadowing something
   * in the parent, this will not iterate over the parent 'x'.  */
  def foreachUnshadowed(
    f: (((From, (To, Option[GenericTypeSet[To]])) => Unit))): Unit = {
    val seenSet = new HashSet[From]()
    map.foreach{ case (name, value) => {
      seenSet.add(name)
      f(name, value)
    }}

    parent.map(_.foreachUnshadowed{ case (name, value) => {
      if (!seenSet.contains(name)) {
        f(name, value)
      }
    } })
  }
}

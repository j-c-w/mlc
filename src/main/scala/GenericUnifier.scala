package toplev

import exceptions._
import scala.collection.mutable.OpenHashMap

/* This is a class that contains a generic unifier.  */

abstract class GenericUnifier[TypeVariable <: GenericPrintable
                                              with GenericType[TypeVariable]] {
  private val map = new OpenHashMap[TypeVariable, TypeVariable](512)

  /* This function MODIFIES THIS UNIFIER!
   * The unifier returned is a new unifier
   * representing both this and the passed unifier.
   */
  def mguSpecialize(other: GenericUnifier[TypeVariable]): Unit = {
    for ((key, value) <- other.map) {
      if (map.contains(key)) {
        // Then we must check that any specialzation is valid.
        // This specializes any sub parts of the type
        // that need to be specialized. This unifier then
        // needs to be unified with this unifier.
        val unifier = specializeTo(map(key), value)
        this mguSpecialize unifier
        specializeNV(key, unifier(map(key)))
      } else {
        specializeNV(key, value)
      }
    }
  }

  def mguUnify(other: GenericUnifier[TypeVariable]): Unit = {
    for ((key, value) <- other.map) {
      if (map.contains(key)) {
        // This specializes any sub parts of the type
        // that need to be specialized. This unifier then
        // needs to be unified with this unifier.
        val unifier = unifyTo(map(key), value)
        this mguUnify unifier
        specializeNV(key, unifier(map(key)))
      } else {
        specializeNV(key, value)
      }
    }
  }

  /* This is like mguUnify, but applies to every element in the list. */
  def mguUnifyAll(other: List[GenericUnifier[TypeVariable]]): Unit = {
    for (unifier <- other) {
      mguUnify(unifier)
    }
  }

  /* This is like mguSpecialze, but applies to every element in the list. */
  def mguSpecializeAll(other: List[GenericUnifier[TypeVariable]]): Unit = {
    for (unifier <- other) {
      mguSpecialize(unifier)
    }
  }

  def selfApply(from: TypeVariable) = {
    // To avoid potential infinite loops here, we count
    // and throw an exception if things get too low.
    var failCount = 10000
    var newType = map(from)
    var tyVars = newType.getTypeVars()
    tyVars = tyVars.filter(x => map.contains(x) && x != from)

    while (tyVars.size > 0) {
      for (tyVar <- tyVars) {
        val introducedTyVars = map(tyVar).getTypeVars()
        newType = newType.substituteFor(tyVar, map(tyVar))
      }

      tyVars = newType.getTypeVars()
      tyVars = tyVars.filter(x => map.contains(x) && x != from)

      failCount -= 1
      if (failCount <= 0) {
        throw new ICE("""Error: Trying to substitue into %s has taken 10000
          substitutions so has been aborted""".format(from.prettyPrint))
      }
    }

    specializeNV(from, newType)
  }

  def apply[TypeEnvClass <: GenericTypeEnv[TypeEnvClass, From, TypeVariable],
            From <: GenericPrintable]
        (env: GenericTypeEnv[TypeEnvClass, From, TypeVariable]) = {
    // We must check every type in the environment.
    env.foreachUnshadowed({ case (name, (typ, qualifiedTypes)) => {
        val atomicList = typ.getTypeVars()
        var newTyp = typ

        for (atomicVar <- atomicList) {
          if (map.contains(atomicVar)) {
            // Make sure all the types in the map are
            // substituted into the atomicVar so that
            // the substitution is complete
            selfApply(atomicVar)
            newTyp = newTyp.substituteFor(atomicVar, map(atomicVar))
          }
        }

        // If this is too slow, this could (in theory) be done
        // with a no validate here.
        // TODO --- MAKE SURE THE TYPES ARE SENSIBLE HERE
        env.updateId(name, newTyp, qualifiedTypes)
    }})
  }

  /*
   * We also provide a per variable type unification.
   * This is required for some intermediate stages in the type
   * checking.
   *
   * This is used as a utility method to implement an
   * environment unification.
   *
   * It takes non-atomic types as 'from'. It will look
   * through the type and make the apppropriate substitutions.
   */
  def apply(from: TypeVariable) = {
    val atomicVars = from.getTypeVars()
    var newType = from

    for (atomicVar <- atomicVars) {
      if (hasType(atomicVar)) {
        // This check is needed so that we only have
        // to do one loop. It runs through and makes
        // sure that the variable in question is not
        // going to reduce to something else in the unifier.
        selfApply(atomicVar)
        newType = newType.substituteFor(atomicVar, map(atomicVar))
      }
    }

    newType
  }

  /*
   * Function specialization is more difficult as it requires
   * preserving the types on the function.
   *
   * This function takes 'from' and 'to' and inserts the mappings
   * required in 'to' to give it a type that is compatable with from.
   *
   * Example usage is:
   *
   *    specializeTo(FunType(int, int), FunType('a, 'a))
   *
   * Would add the mappings 'a -> int in.
   *
   * But, something like
   *
   *    specializeTo(FunType('a, 'a), FunType(int, int))
   *
   * adds the EMPTY unifier. (since the 'to' argument does not
   * need to be specialized).
   *
   * This does not modify the state, but returns a new
   * unifier that (can) be unified with this one.
   */
  def specializeTo(from: TypeVariable,
                   to: TypeVariable): GenericUnifier[TypeVariable]

  /* This is the same as 'specializeTo', but it unifies
   * (i.e. bi-directional rather than unidirectional)
   *
   * This does not modify the state, but returns a new
   * unifier that (can) be unified with this one.
   */
  def unifyTo(from: TypeVariable,
              to: TypeVariable): GenericUnifier[TypeVariable]

  /*
   * This must do two things. It must first check whether the specialization
   * makes sense. It must further check that the specialization is on
   * atomic types only (i.e. 'a -> int). All other unifications make no
   * sense
   */
  def isValidSpecialization(from: TypeVariable, to: TypeVariable): Boolean

  // This function adds the specialization TYP -> TYP into the 
  // unifier.
  def specializeVerify(from: TypeVariable, to: TypeVariable) =
    if (isValidSpecialization(from, to))
      specializeNV(from, to)
    else
      throw new SpecializationError(from, to)

  // This adds the specilaiztion Typ -> Typ into the unifier
  // without verification that it is a sensible thing to do.
  //
  // Only atomic specializations are allowed.
  //
  // If this would introduce a cycle in the unifier, then we deal
  // with that issue by flipping from and to.
  def specializeNV(from: TypeVariable, to: TypeVariable) = {
    // Check if this would introduce a loop.  If so, then don't insert it.
    if (completesLoop(from, to)) {
      // Don't want to create loops.  This is sound provided that the
      // type pass is correctly handling the unification direction.
    } else {
      map(from) = to
    }
  }

  private def completesLoop(from: TypeVariable, to: TypeVariable): Boolean = {
    var maxIters = 10000

    if (from == to) {
      return true
    }

    var tail = to
    while (map.contains(tail)) {
      if (from == map(tail)) {
        return true
      }

      maxIters -= 1

      if (maxIters < 0) {
        throw new ICE("Found a loop of more that 10000 type variables long")
      }

      tail = map(tail)
    }

    return false
  }

  private def hasType(typ: TypeVariable) =
    map.contains(typ)

  def mapSize() = map.size

  def prettyPrint =  """Unifying
  %s
  """.format(map.map{ case (from, to) =>
      from.prettyPrint +  " -> " + to.prettyPrint }.mkString("\n"))
}

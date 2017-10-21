package toplev

import scala.collection.mutable.HashMap
import exceptions.ICE

/* This is a class that contains a generic unifier.  */

abstract class GenericUnifier[TypeVariable <: GenericPrintable
                                              with GenericType[TypeVariable]] {
  private val map = new HashMap[TypeVariable, TypeVariable]()

  /* This function MODIFIES THIS UNIFIER!
   * The unifier returned is a new unifier
   * representing both this and the passed unifier.
   */
  def mgu(other: GenericUnifier[TypeVariable]): Unit = {
    for ((key, value) <- other.map) {
      if (map.contains(key)) {
        // Then we must check that any specialzation is valid.
        val enteredType = unify(value, map(key))

        specializeNV(key, enteredType)
      } else {
        specializeNV(key, value)
      }
    }
  }

  def apply[TypeEnvClass, From <: GenericPrintable]
        (env: GenericTypeEnv[TypeEnvClass, From, TypeVariable]) = {
                                                      ???
  }

  /*
   * This must do two things. It must first check whether the specialization
   * makes sense. It must further check that the specialization is on
   * atomic types only (i.e. 'a -> int). All other unifications make no
   * sense
   */
  def isValidSpecialization(from: TypeVariable, to: TypeVariable): Boolean

  // This is a function that is used locally to implement the MGU algorithm.
  // It is generally implementable by a single large case statement.
  // This is allowed (and expected) to fail.
  protected def unify(t: TypeVariable, u: TypeVariable): TypeVariable

  // This function adds the specialization TYP -> TYP into the 
  // unifier.
  def specialize(from: TypeVariable, to: TypeVariable) = {
    if (isValidSpecialization(from, to))
      specializeNV(from, to)
    else
      throw new ICE("Error: Specialize verification failed. This is a bug.")
  }

  // This adds the specilaiztion Typ -> Typ into the unifier
  // without verification that it is a sensible thing to do.
  //
  // Only atomic specializations are allowed.
  def specializeNV(from: TypeVariable, to: TypeVariable) = {
    map(from) = to
  }
}

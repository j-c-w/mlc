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
        // val enteredType = unify(value, map(key))

        specializeNV(key, value)
      } else {
        specializeNV(key, value)
      }
    }

    selfApply()
  }

  /* This function applies the map to itself, removing any redundancies.
   *
   * For example, in the map:
   *
   *  'a -> int
   *  'b -> 'a list
   *
   * This updates the map with:
   *
   *  'a -> int
   *  'b -> int list
   *
   * It is costly O(n^2), so is only applied when it has to be.
   */
  private def selfApply(): Unit = {
    for ((key, value) <- map) {
      for ((otherKey, otherValue) <- map) {
        if (otherValue.contains(key)) {
          map(otherKey) = otherValue.substitueFor(key, value)
        }
      }
    }
  }

  def apply[TypeEnvClass, From <: GenericPrintable]
        (env: GenericTypeEnv[TypeEnvClass, From, TypeVariable]) = {
    selfApply()
    for ((key, value) <- map) {
      // We get all the elements of the map that are sufficiently
      // set.

      // A trivial optimization if this is running too slowly is
      // to not recompute this every time and just compute the differences
      // in the maps
      //
      // TODO -- TAKE NOTE OF POLYTYPES HERE
      env.map.map{ case (k, e) => (k, e) }.foreach[Unit] {
        case (envKey: From, envValue: TypeVariable) =>
          if (envValue == value) {
            env.add(envKey, value)
          }
      }
    }
  }

  /*
   * We also provide a per variable type unification.
   * This is required for some intermediate stages in the type
   * checking
   */
  def apply(from: TypeVariable) = {
    if (hasType(from))
      map(from)
    else
      // This is the chosen semantics here because it means that
      // if this map specializes the type, then it specializes it,
      // and otherwise it does nothing.
      from
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
   */
  def specializeTo(from: TypeVariable, to: TypeVariable): Unit

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

  // This adds the specilaiztion Typ -> Typ into the unifier
  // without verification that it is a sensible thing to do.
  //
  // Only atomic specializations are allowed.
  def specializeNV(from: TypeVariable, to: TypeVariable) = {
    map(from) = to
  }

  private def hasType(typ: TypeVariable) =
    map.contains(typ)

  def mapSize() = map.size

  def prettyPrint =  """Unifying
  %s
  """.format(map.map{ case (from, to) =>
      from.prettyPrint +  " -> " + to.prettyPrint }.mkString("\n"))
}

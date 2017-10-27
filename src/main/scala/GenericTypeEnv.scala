package toplev

import scala.collection.mutable.{Map,HashMap}

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

  /* Every type in this map can be used either as a type quantified
   * here or a type quantified elsewhere.
   *
   * Types that are quantified at this level of the type environment
   * are returned by typeCloning all of the subtypes in that type.
   */
  private val map: Map[From, (To, Option[TypeClassSet[To]])] =
    HashMap[From, (To, Option[TypeClassSet[To]])]()

  def prettyPrint = """

  %s

  """.format(map.map(pair => pair._1.prettyPrint + ": " +
                     pair._2._1.prettyPrint).mkString("\n"))

  def hasType(id: From): Boolean =
    map.contains(id) || parent.map(_.hasType(id)).getOrElse(false)

  def add(id: From, typ: To, qualified: Boolean): Unit =
    if (qualified)
      add(id, typ, Some(typ.getTypeVars()))
    else
      add(id, typ, None)

  def add(id: From, typ: To, qualifiedTypes: Option[TypeClassSet[To]]): Unit = {
    map(id) = (typ, qualifiedTypes)
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
               quantifiedTypes: Option[TypeClassSet[To]]): Unit = {
    if (getOrFail(id).specializesTo(newTyp))
      updateIdNoValidate(id, newTyp, quantifiedTypes)
    else
      throw new TypeAssignmentException()
  }

  /* This attempts to update types in the parent
   * if possible.
   */
  def updateIdNoValidate(id: From, newTyp: To,
                         qualifiedTypes: Option[TypeClassSet[To]]): Unit = {
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
the environment""".format(id.prettyPrint)))
  }

  /* This returns all the unquantified types for some variable
   * in the map.
   *
   * This is used for unification to unify only unquantified types.
   */
  def getUnquantifiedTypesFor(id: From): TypeClassSet[To] = {
    val (typ, quantified) = map(id)

    quantified match {
      case None => typ.getTypeVars()
      case Some(quantified) => typ.getTypeVars() - quantified
    }
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

  def foreach(f : (((From, (To, Option[TypeClassSet[To]]))) => Unit)): Unit = {
    map.foreach(f)
    parent.map(_.foreach(f))
  }
}

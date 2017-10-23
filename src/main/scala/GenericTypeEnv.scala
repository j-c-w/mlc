package toplev

import scala.collection.mutable.{Map,HashMap}

import exceptions._
/*
 *
 * This is a generic type environment. It provides
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

  val map: Map[From, To] = HashMap[From, To]()

  def prettyPrint = """

  %s

  """.format(map.map(pair => pair._1.prettyPrint + ": " + pair._2.prettyPrint)
             .mkString("\n"))

  def hasType(id: From): Boolean =
    map.contains(id) || parent.map(_.hasType(id)).getOrElse(false)

  def add(id: From, typ: To): Unit = {
    map(id) = typ
  }

  /* This function automatically validates that the variable it is
   * replacing is OK to replace with the one replacing it.
   *
   * Throw a runtime exception if that is not OK.
   *
   * This should be used unless you are really sure that it is OK.
   */
  def updateId(id: From, newTyp: To): Unit = {
    if (map(id).specializesTo(newTyp))
      map(id) = newTyp
    else
      throw new TypeAssignmentException()
  }

  def updateIdNoValidate(id: From, newTyp: To): Unit = {
    map(id) = newTyp
  }

  /* This method finds all instances of 'oldTyp' and replaces
   * them with instances of 'newTyp'.
   */
  def updateTyp(oldTyp: To, newTyp: To): Unit = {

  }

  def get(id: From): Option[To] =
    map.get(id)

  def apply(id: From): Option[To] =
    get(id)
}

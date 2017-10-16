package toplev

import scala.collection.mutable.{Map,HashMap}
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

abstract class GenericTypeEnv[TypeEnvClass, From <: GenericPrintable,
                              To <: GenericPrintable]
               (val parent: Option[GenericTypeEnv[TypeEnvClass, From, To]]) {
  def this() = this(None)

  private val map: Map[From, To] = HashMap[From, To]()

  def prettyPrint = """

  %s

  """.format(map.map(pair => pair._1.prettyPrint + ": " + pair._2.prettyPrint)
             .mkString("\n"))

  def hasType(id: From): Boolean = 
    map.contains(id) || parent.map(_.hasType(id)).getOrElse(false)
  
  // Todo -- implement more methods as required. Will definitely need
  // a set function. A get function is likely to be needed for function
  // specialization.
}

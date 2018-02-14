package ast_change_names

import scala.collection.mutable.{Map,HashMap,Set,HashSet}
import exceptions.{BadPatternException,UnrecognizedIdentifierError}

class ASTNameEnvironment(val parent: Option[ASTNameEnvironment]) {
  val map: Map[String, String] = new HashMap[String, String]()
  val datatypeNames: Set[String] = new HashSet[String]()

  def this() = this(None)
  def this(parent: ASTNameEnvironment) = this(Some(parent))

  def addDataType(from: String, to: String) = {
    datatypeNames += from
    map(from) = to
  }

  def add(from: String, to: String) =
    map(from) = to

  def isDataType(name: String): Boolean =
    if (datatypeNames.contains(name))
      true
    else 
      parent match {
        case Some(parentEnv) => parentEnv.isDataType(name)
        case None => false
      }

  /* This adds the variable if it is new at this level. Otherwise
   * it fails.  */
  def addIfNew(from: String, to: String) =
    if (map.contains(from)) {
      throw new BadPatternException(
        "Duplicate variable %s in pattern".format(from))
    } else {
      add(from, to)
    }

  def get(from: String): String = {
    if (map.contains(from)) {
      map(from)
    } else {
      parent match {
        case Some(parentEnv) => parentEnv.get(from)
        case None => throw new UnrecognizedIdentifierError("""
          |Identifier %s not defined.""".stripMargin.format(from))
      }
    }
  }

  def contains(from: String): Boolean = {
    if (map.contains(from))
      true
    else
      parent match {
        case Some(parentEnv) => parentEnv.contains(from)
        case None => false
      }
  }

  def apply(from: String) = get(from)
}

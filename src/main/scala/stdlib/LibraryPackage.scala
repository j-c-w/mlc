package stdlib

import frontend._

abstract class LibraryPackage {
  def apply(name: List[String]): Option[ASTType]

  /* This should list all the prefixes with which to
  *  refer to a certain package. e.g. for Real.fromInt,
  *  'Real' would be the package, and 'fromInt' the name. */
  val prefixesAccepted: List[List[String]]
}

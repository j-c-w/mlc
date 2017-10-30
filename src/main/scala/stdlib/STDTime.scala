package stdlib

import frontend._

object STDTime extends LibraryPackage {
  def apply(name: List[String]): Option[ASTType] = name match {
    case List("toString") =>
      Some(ASTFunctionType(
        ASTRealType(),
        ASTStringType()))
    case _ => None
  }

  val prefixesAccepted = List(List("Time"))
}

package stdlib

import frontend._

object STDReal extends LibraryPackage {
  def apply(name: List[String]): Option[ASTType] = name match {
    case List("fromInt") => Some(ASTTypeFunction(ASTIntType(), ASTRealType()))
    case List("toString") =>
      Some(ASTTypeFunction(ASTRealType(), ASTStringType()))
    case _ => None
  }

  val prefixesAccepted = List(List("Real"))
}

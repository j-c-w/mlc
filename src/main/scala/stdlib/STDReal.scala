package stdlib

import frontend._

object STDReal extends LibraryPackage {
  def apply(name: List[String]): Option[ASTType] = name match {
    case List("fromInt") => Some(ASTFunctionType(ASTIntType(), ASTRealType()))
    case List("toString") =>
      Some(ASTFunctionType(ASTRealType(), ASTStringType()))
    case List("toInt") => Some(ASTFunctionType(ASTRealType(), ASTIntType()))
    case _ => None
  }

  val prefixesAccepted = List(List("Real"))
}

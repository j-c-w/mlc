package stdlib

import frontend._

object STDMath extends LibraryPackage {
  def apply(name: List[String]): Option[ASTType] = name match {
    case List("cos") | List("sin") | List("sqrt") | List("atan") =>
      Some(ASTFunctionType(
        ASTRealType(),
        ASTRealType()))
    case _ => None
  }

  val prefixesAccepted = List(List("Math"))
}

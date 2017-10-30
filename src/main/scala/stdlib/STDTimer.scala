package stdlib

import frontend._

object STDTimer extends LibraryPackage {
  def apply(name: List[String]): Option[ASTType] = name match {
    case List("startRealTimer") =>
      Some(ASTFunctionType(
        ASTUnitType(),
        ASTRealType()))
    case List("checkRealTimer") =>
      Some(ASTFunctionType(
        ASTRealType(),
        ASTRealType()))
    case _ => None
  }

  val prefixesAccepted = List(List("Timer"))
}

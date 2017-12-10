package byteR

import toplev.GenericPrintable

case class JVMMethod(var name: String, var arguments: List[JVMType],
                     var result: JVMType, var body: List[JVMInstruction],
                     var isStatic: Boolean)
    extends GenericPrintable {
  def prettyPrint = """.method public %s %s : (%s)%s
  |     %s
  |     %s
  |  .end method""".stripMargin.format(staticModifier, name,
                                       arguments.map(
                                         _.prettyPrint).mkString(";"),
                                       result.prettyPrint,
                                       body.map(
                                         _.prettyPrint).mkString("\n     "),
                                       returnInstr.map(
                                         _.prettyPrint).mkString("\n     "))
  private def staticModifier: String =
    if (isStatic) "static"
    else ""

  private def returnInstr: List[JVMInstruction] = result match {
    case result: JVMPrimitiveType => result match {
      case JVMIntPrimitiveType() => List(JVMIReturn())
      case JVMFloatPrimitiveType() => List(JVMFReturn())
      case JVMCharPrimitiveType() => List(JVMIReturn())
      case JVMVoidPrimitiveType() => List(JVMPop(), JVMVReturn())
    }
    case other => List(JVMAReturn())
  }
}

case class JVMMethodRef(var classRef: JVMClassRef, var name: String,
                        var argType: List[JVMType],
                        var resType: JVMType) extends GenericPrintable {
  def prettyPrint =
    " %s %s (%s)%s".format(classRef.prettyPrint, name,
                           argType.map(_.prettyPrint).mkString(""),
                           resType.prettyPrint)

  def countArgs: Int = argType.length
}

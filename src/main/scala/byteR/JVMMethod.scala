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
                                         _.prettyPrint).mkString(""),
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
      case JVMBooleanPrimitiveType() => List(JVMIReturn())
      // For void returns, we expect the generating function
      // to have removed the return.
      case JVMVoidPrimitiveType() => List(JVMVReturn())
    }
    case other => List(JVMAReturn())
  }
}

case class JVMMethodRef(var classRef: JVMClassRef, var name: String,
                        var argType: List[JVMType],
                        var resType: JVMType) extends GenericPrintable {
  def canSideEffect =
    (classRef, name) match {
      case (boxedRef: JVMBoxedRef, "valueOf") => false
      case (boxedRef: JVMBoxedRef, value) =>
        value.endsWith("Value")
      // Not all STD lib references are immutable.
      case (libRef: JVMCMLCLibRef, funCall) => libRef match {
        case JVMLinkedListRef() => false
        case JVMLinkedListNilRef() => false
        case JVMUnitRef() => false
        case JVMMatchExceptionRef() => false
        case JVMDataTypeClassRef() => true
        case JVMExceptionClassRef() => true
        case JVMThrowableClassRef() => false
        case JVMTupleRef() =>
          funCall match {
            case "get" => false
            case "equals" => false
            case "set" => true
            case _ => true
          }
        case JVMFunctionRef() => true
      }
        false
      case _ => true
    }

  def prettyPrint =
    " %s %s (%s)%s".format(classRef.prettyPrint, name,
                           argType.map(_.prettyPrint).mkString(""),
                           resType.prettyPrint)

  def countArgs: Int = argType.length
}

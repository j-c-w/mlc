package byteR

import toplev.GenericPrintable

case class JVMField(var name: String, var typ: JVMType, var isStatic: Boolean)
    extends GenericPrintable {
  def prettyPrint =
    ".field %s %s %s".format(getStaticModifier, name, typ.prettyPrint)

  private def getStaticModifier =
    if (isStatic) "static" else ""
}

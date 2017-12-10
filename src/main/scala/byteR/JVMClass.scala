package byteR

import toplev.GenericPrintable

case class JVMClass(var name: String, var superClass: Option[JVMClassRef],
                    var fields: List[JVMField], var methods: List[JVMMethod])
    extends GenericPrintable {
  def prettyPrint = """.class %s
  %s

  %s

  %s

.end class
  """.format(name, superClass.map(".super " + _.prettyPrint).getOrElse(""),
             fields.map(_.prettyPrint).mkString("\n  "),
             methods.map(_.prettyPrint).mkString("\n\n  "))
}

object JVMObject extends JVMClass("java/lang/Object", None, List(), List())

package byteR

import toplev.GenericPrintable

object JVMClassRef {
  def classRefFor(name: String): JVMCustomClassRef = {
    new JVMCustomClassRef(name)
  }
}

trait JVMClassRef extends GenericPrintable

case class JVMCustomClassRef(var name: String) extends JVMClassRef {
  def prettyPrint = name
}

case class JVMMainClassRef() extends JVMClassRef {
  def prettyPrint = "Main"
}

case class JVMObjectRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Object"
}

case class JVMIntegerRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Integer"
}

case class JVMBooleanRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Boolean"
}

case class JVMFloatRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Float"
}

case class JVMStringRef() extends JVMClassRef {
  def prettyPrint = "java/lang/String"
}

case class JVMCharacterRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Character"
}

case class JVMFunctionRef() extends JVMClassRef {
  def prettyPrint = "cmlc/Function"
}

case class JVMUnitRef() extends JVMClassRef {
  def prettyPrint = "cmlc/Unit"
}

case class JVMLinkedListRef() extends JVMClassRef {
  def prettyPrint = "cmlc/LinkedList"
}

case class JVMLinkedListNilRef() extends JVMClassRef {
  def prettyPrint = "cmlc/Nil"
}

case class JVMMatchExceptionRef() extends JVMClassRef {
  def prettyPrint = "cmlc/MatchError"
}

case class JVMTupleRef() extends JVMClassRef {
  def prettyPrint = "cmlc/Tuple"
}

// These are here because we some things (such as print) as builtins.  Normal
// standard library references are dealt wih in the STDLib package.
case class JVMSystemRef() extends JVMClassRef {
  def prettyPrint = "java/lang/System"
}

case class JVMPrintStreamRef() extends JVMClassRef {
  def prettyPrint = "java/io/PrintStream"
}

case class JVMStringBuilderRef() extends JVMClassRef {
  def prettyPrint = "java/lang/StringBuilder"
}

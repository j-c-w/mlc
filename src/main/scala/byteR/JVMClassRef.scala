package byteR

import toplev.GenericPrintable

object JVMClassRef {
  def classRefFor(name: String): JVMCustomClassRef = {
    new JVMCustomClassRef(name)
  }
}

sealed trait JVMClassRef extends GenericPrintable

sealed trait JVMBoxedRef extends JVMClassRef

sealed trait JVMCMLCLibRef extends JVMClassRef

case class JVMCustomClassRef(var name: String) extends JVMClassRef {
  def prettyPrint = name
}

case class JVMMainClassRef() extends JVMClassRef {
  def prettyPrint = "Main"
}

case class JVMObjectRef() extends JVMClassRef {
  def prettyPrint = "java/lang/Object"
}

case class JVMIntegerRef() extends JVMBoxedRef {
  def prettyPrint = "java/lang/Integer"
}

case class JVMBooleanRef() extends JVMBoxedRef {
  def prettyPrint = "java/lang/Boolean"
}

case class JVMFloatRef() extends JVMBoxedRef {
  def prettyPrint = "java/lang/Float"
}

case class JVMStringRef() extends JVMClassRef {
  def prettyPrint = "java/lang/String"
}

case class JVMCharacterRef() extends JVMBoxedRef {
  def prettyPrint = "java/lang/Character"
}

case class JVMFunctionRef() extends JVMCMLCLibRef {
  def prettyPrint = "cmlc/Function"
}

case class JVMUnitRef() extends JVMCMLCLibRef {
  def prettyPrint = "cmlc/Unit"
}

case class JVMLinkedListRef() extends JVMCMLCLibRef {
  def prettyPrint = "cmlc/LinkedList"
}

case class JVMLinkedListNilRef() extends JVMCMLCLibRef {
  def prettyPrint = "cmlc/Nil"
}

case class JVMMatchExceptionRef() extends JVMCMLCLibRef {
  def prettyPrint = "cmlc/MatchError"
}

case class JVMTupleRef() extends JVMCMLCLibRef {
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

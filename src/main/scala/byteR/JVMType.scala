package byteR

import exceptions.ICE
import toplev.GenericPrintable

trait JVMType extends GenericPrintable {
  def getRefFor: JVMClassRef

  def getSizeBytes: Int
}

trait JVMPrimitiveType extends JVMType {
  def getRefFor =
    throw new ICE("Cannot get ref of a primitive type")
}

case class JVMVoidPrimitiveType() extends JVMPrimitiveType {
  def prettyPrint = "V"

  def getSizeBytes = 4
}

case class JVMIntPrimitiveType() extends JVMPrimitiveType {
  def prettyPrint = "I"

  def getSizeBytes = 4
}

case class JVMFloatPrimitiveType() extends JVMPrimitiveType {
  def prettyPrint = "F"

  def getSizeBytes = 4
}

case class JVMCharPrimitiveType() extends JVMPrimitiveType {
  def prettyPrint = "C"

  def getSizeBytes = 4
}

case class JVMBooleanPrimitiveType() extends JVMPrimitiveType {
  def prettyPrint = "Z"

  def getSizeBytes = 4
}

trait JVMRefType extends JVMType {
  def getSizeBytes = 4
}

/* Most classes shuld be accessed through the JCMClassType
 * variable.  However, these two are special cases.  */
case class JVMObjectType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/Object;"

  def getRefFor = new JVMObjectRef()
}

case class JVMStringType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/String;"

  def getRefFor = new JVMStringRef()
}

case class JVMCharacterType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/Character;"

  def getRefFor = new JVMCharacterRef()
}

case class JVMBooleanType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/Boolean;"

  def getRefFor = new JVMBooleanRef()
}

case class JVMArrayType(val elemsType: JVMType) extends JVMRefType {
  def prettyPrint = "[" + elemsType.prettyPrint

  // We /could/ have a ref type for a (say) ArrayList, but that is not
  // what this represents.  Arrays are effectively primitives.
  def getRefFor = throw new ICE("JVMArray type cannot have a ref type")
}

case class JVMTupleType() extends JVMRefType {
  def prettyPrint = "Lcmlc/Tuple;"

  def getRefFor = new JVMTupleRef()
}

case class JVMClassType(var name: JVMClassRef) extends JVMRefType {
  def prettyPrint = "L" + name.prettyPrint + ";"

  def getRefFor = name
}

case class JVMIntegerType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/Integer;"

  def getRefFor = new JVMIntegerRef()
}

case class JVMFloatType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/Float;"

  def getRefFor = new JVMFloatRef()
}

case class JVMUnitType() extends JVMRefType {
  def prettyPrint = "Lcmlc/Unit;"

  def getRefFor = new JVMUnitRef()
}

case class JVMFunctionType() extends JVMRefType {
  def prettyPrint = "Lcmlc/Function;"

  def getRefFor = new JVMFunctionRef()
}

case class JVMLinkedListType() extends JVMRefType {
  def prettyPrint = "Lcmlc/LinkedList;"

  def getRefFor = new JVMLinkedListRef()
}

// These are here because we treat some things (such as "print") as builtins.
// Most references needed for the standard library are in the STDLib package.

case class JVMPrintStreamType() extends JVMRefType {
  def prettyPrint = "Ljava/io/PrintStream;"

  def getRefFor = new JVMPrintStreamRef()
}

case class JVMStringBuilderType() extends JVMRefType {
  def prettyPrint = "Ljava/lang/StringBuilder;"

  def getRefFor = new JVMStringBuilderRef()
}

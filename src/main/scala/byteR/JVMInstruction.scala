package byteR

import toplev.GenericPrintable

sealed trait JVMInstruction extends GenericPrintable {
  def stackEffect: Int

  /* This should return true if this is an instruction with some implicit
   * effect.  It should be conservative, i.e.  it MUST not say that an
   * instruction with a side effect has none, but it may say that an
   * instruction with no side effect has some.  */
  def hasSideEffect: Boolean
}

sealed trait JVMUnaryInstruction extends JVMInstruction {
  def stackEffect = 0
}

sealed trait JVMBinaryInstruction extends JVMInstruction {
  // Pops 2, but pushes a result.
  def stackEffect = -1
}

sealed trait JVMPushInstruction extends JVMInstruction {
  def stackEffect = 1
}

sealed trait JVMLabelInstruction extends JVMInstruction

/* This trait is for all loads from variables that can be treated
 * as variables for the sake of optimizations.  */
sealed trait JVMLoadInstruction extends JVMPushInstruction {
  def getVariable: JVMVariable
}

/* Likewise for stores.  */
sealed trait JVMStoreInstruction extends JVMInstruction {
  def stackEffect = -1

  def getVariable: JVMVariable
}

sealed trait JVMJumpInstruction extends JVMInstruction {
  def getTarget: JVMLabel
}

sealed trait JVMConditionalJumpInstruction extends JVMJumpInstruction

// This is for walks of the tree to emit warnings if they are missing
// instructions with children.  It is for instructions with non-primitive
// children.
sealed trait JVMParentInstruction extends JVMInstruction

sealed trait JVMCompareAndJumpInstruction
    extends JVMConditionalJumpInstruction {
  def stackEffect = -2
}

case object JVMNoInstruction extends JVMInstruction {
  def stackEffect = 0

  def hasSideEffect = false

  def prettyPrint = ""
}

case class JVMCheckCast(to: JVMClassRef) extends JVMUnaryInstruction 
    with JVMParentInstruction {
  def hasSideEffect = false

  def prettyPrint = "checkcast " + to.prettyPrint
}

case class JVMInvokeSpecialMethod(method: JVMMethodRef)
    extends JVMInstruction with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "invokespecial Method " + method.prettyPrint

  // This instruction does not push anything new onto the stack.
  // But it also eats a referece
  def stackEffect = - method.countArgs - 1
}

case class JVMInvokeVirtualMethod(method: JVMMethodRef)
    extends JVMInstruction with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "invokevirtual Method " + method.prettyPrint

  // This instruction eats all the arguments, eats a reference
  // and puts a result on the stack (+1 -1)
  // Unless it is a void method, in which case nothing new is
  // put on the stack.
  def stackEffect = - method.countArgs + (method.resType match {
    case JVMVoidPrimitiveType() => -1
    case other => 0
  })
}

case class JVMInvokeStaticMethod(method: JVMMethodRef) extends JVMInstruction
    with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "invokestatic Method " + method.prettyPrint

  def stackEffect = - method.countArgs + (method.resType match {
    // If this is void then we do not push anything new onto the stack.
    case JVMVoidPrimitiveType() => 0
    case other => 1
  })
}

case class JVMGetField(jvmClass: JVMClassRef, name: String, typ: JVMType)
    extends JVMParentInstruction with JVMLoadInstruction {
  def hasSideEffect = false

  def prettyPrint = "getfield Field " + jvmClass.prettyPrint + " " +
    name + " " + typ.prettyPrint

  def getVariable =
    new JVMMemberVariable(jvmClass.prettyPrint + "." + name)

  // In exchange for getting a value, this eats a reference.
  override def stackEffect = 0
}

case class JVMPutField(jvmClass: JVMClassRef, name: String, typ: JVMType)
    extends JVMStoreInstruction with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "putfield Field " + jvmClass.prettyPrint + " " +
    name + " " + typ.prettyPrint

  def getVariable =
    new JVMMemberVariable(jvmClass.prettyPrint + "." + name)

  // In addition to the value, this eats a reference.
  override def stackEffect = -2
}

case class JVMGetStaticField(jvmClass: JVMClassRef, name: String, typ: JVMType)
    extends JVMParentInstruction with JVMLoadInstruction {
  def hasSideEffect = false

  def prettyPrint = "getstatic Field " + jvmClass.prettyPrint + " " +
    name + " " + typ.prettyPrint

  def getVariable =
    new JVMStaticVariable(jvmClass.prettyPrint + "." + name)
}

case class JVMPutStaticField(jvmClass: JVMClassRef, name: String, typ: JVMType)
    extends JVMStoreInstruction with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "putstatic Field " + jvmClass.prettyPrint + " " +
    name + " " + typ.prettyPrint

  def getVariable =
    new JVMStaticVariable(jvmClass.prettyPrint + "." + name)
}

case class JVMNew(var jvmClass: JVMClassRef) extends JVMPushInstruction
    with JVMParentInstruction {
  def hasSideEffect = false

  def prettyPrint = "new " + jvmClass.prettyPrint
}

sealed trait JVMReturn extends JVMInstruction {
  // In reality, return instructions will clear the stack.  However,
  // for the soundness of the compiler, we require that the stack is
  // empty on a return anyway.
  //
  // This constrains the return instruction to only pop off what it returns.
  def stackEffect = -1
}

case class JVMVReturn() extends JVMReturn {
  def hasSideEffect = true

  def prettyPrint = "return"
}

case class JVMAReturn() extends JVMReturn {
  def hasSideEffect = true

  def prettyPrint = "areturn"
}

case class JVMFReturn() extends JVMReturn {
  def hasSideEffect = true

  def prettyPrint = "freturn"
}

case class JVMIReturn() extends JVMReturn {
  def hasSideEffect = true

  def prettyPrint = "ireturn"
}

case class JVMAThrow() extends JVMInstruction {
  def hasSideEffect = true

  def prettyPrint = "athrow"

  // Of course, this makes little sense.  The stackEffect field is used to
  // ensure the soundness of the stack and ensure that the right stack depth
  // is calculated.
  //
  // The idea here is that even though throw never returns anything in
  // itself, there 'would' be something here. So, the sum is:
  // -1 for the throw, +1 for the imaginary unit that gets put on the
  // stack
  def stackEffect = -1
}

case class JVMJump(var dest: JVMLabel) extends JVMLabelInstruction
    with JVMJumpInstruction
    with JVMParentInstruction {
  def hasSideEffect = true

  def prettyPrint = "goto " + dest.prettyPrint

  def stackEffect = 0

  def getTarget = dest
}

case class JVMLabelMark(var label: JVMLabel) extends JVMLabelInstruction
    with JVMParentInstruction {
  def hasSideEffect = false

  def prettyPrint = label.prettyPrint + ":"

  def stackEffect = 0
}

case class JVMIPush(var value: Int) extends JVMPushInstruction {
  def hasSideEffect = false

  def prettyPrint = value match {
    case -1 => "iconst_m1"
    case 0 => "iconst_0"
    case 1 => "iconst_1"
    case 2 => "iconst_2"
    case 3 => "iconst_3"
    case 4 => "iconst_4"
    case n if n.isValidByte => "bipush " + n.toString
    case n if n.isValidShort => "sipush " + n.toString
    case n => "ldc " + value.toString
  }
}

case class JVMFPush(var float: Float) extends JVMPushInstruction {
  def hasSideEffect = false

  def prettyPrint = "ldc " + assemblerRepresentation

  private lazy val assemblerRepresentation =
    if (float.isPosInfinity) {
      "+Infinityf"
    } else if (float.isNegInfinity) {
      "-Infinityf"
    } else if (float.isNaN) {
      // Load all NaNs as positive.
      "+NaNf"
    } else {
      float.toString + "f"
    }
}

case class JVMLDCString(var string: String) extends JVMPushInstruction {
  def hasSideEffect = false

  // This class assumes that the string has already been escaped.
  def prettyPrint = "ldc " + string.toString
}

case class JVMNullPush() extends JVMPushInstruction {
  def hasSideEffect = false

  def prettyPrint = "aconst_null"
}

case class JVMLocalAStore(n: Int) extends JVMStoreInstruction {
  def hasSideEffect = true

  // N = 0 is the 'this' pointer. Do not overwrite that.
  assert(n > 0)

  def getVariable =
    new JVMLocalVariable(n)

  def prettyPrint = n match {
    case 1 => "astore_1"
    case 2 => "astore_2"
    case 3 => "astore_3"
    case n => "astore " + n.toString
  }
}

/* This is only for use in static methods.  In instance methods,
 * it will overwrite the pointer.  */
case class JVMSelfStore() extends JVMStoreInstruction {
  def hasSideEffect = true

  def prettyPrint = "astore_0"

  def getVariable = new JVMLocalVariable(0)
}

case class JVMLocalALoad(n: Int) extends JVMLoadInstruction {
  def hasSideEffect = false

  // N = 0 is the 'this' pointer.  Use JVMSelfLoad() for that.
  assert(n > 0)

  def prettyPrint = n match {
    case 1 => "aload_1"
    case 2 => "aload_2"
    case 3 => "aload_3"
    case n => "aload " + n.toString
  }

  def getVariable = new JVMLocalVariable(n)
}

case class JVMSelfLoad() extends JVMLoadInstruction {
  def hasSideEffect = false

  def prettyPrint = "aload_0"

  def getVariable = new JVMLocalVariable(0)
}

case class JVMFNeg() extends JVMUnaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fneg"
}

case class JVMFAdd() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fadd"
}

case class JVMFSub() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fsub"
}

case class JVMFDiv() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fdiv"
}

case class JVMFMul() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fmul"
}

case class JVMINeg() extends JVMUnaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "ineg"
}

case class JVMIAdd() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "iadd"
}

case class JVMISub() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "isub"
}

case class JVMIDiv() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "idiv"
}

case class JVMIMul() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "imul"
}

case class JVMIRem() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "irem"
}

case class JVMIAnd() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "iand"
}

case class JVMIOr() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "ior"
}

case class JVMDup() extends JVMPushInstruction {
  def hasSideEffect = false

  def prettyPrint = "dup"
}

case class JVMDup2X1() extends JVMInstruction {
  def hasSideEffect = true

  def prettyPrint = "dup2_x1"

  def stackEffect = 2
}

case class JVMPop() extends JVMInstruction {
  def hasSideEffect = false

  def prettyPrint = "pop"

  def stackEffect = -1
}

case class JVMPop2() extends JVMInstruction {
  def hasSideEffect = false

  def prettyPrint = "pop2"

  def stackEffect = -2
}

case class JVMSwap() extends JVMInstruction {
  def hasSideEffect = true

  def prettyPrint = "swap"

  def stackEffect = 0
}

case class JVMIfIntCmpEq(var branchTarget: JVMLabel)
    extends JVMLabelInstruction with JVMParentInstruction
    with    JVMCompareAndJumpInstruction {
  def hasSideEffect = true

  def prettyPrint = "if_icmpeq " + branchTarget.prettyPrint

  def getTarget = branchTarget
}

case class JVMIfIntCmpLEQ(var branchTarget: JVMLabel)
    extends JVMLabelInstruction with JVMParentInstruction
    with    JVMCompareAndJumpInstruction {
  def hasSideEffect = true

  def prettyPrint = "if_icmple " + branchTarget.prettyPrint

  def getTarget = branchTarget
}

case class JVMIfIntCmpGEQ(var branchTarget: JVMLabel)
    extends JVMLabelInstruction with JVMParentInstruction
    with    JVMCompareAndJumpInstruction {
  def hasSideEffect = true

  def prettyPrint = "if_icmpge " + branchTarget.prettyPrint

  def getTarget = branchTarget
}

case class JVMIfIntCmpLT(var branchTarget: JVMLabel)
    extends JVMLabelInstruction with JVMParentInstruction
    with    JVMCompareAndJumpInstruction {
  def hasSideEffect = true

  def prettyPrint = "if_icmplt " + branchTarget.prettyPrint

  def getTarget = branchTarget
}

case class JVMIfIntCmpGT(var branchTarget: JVMLabel)
    extends JVMLabelInstruction with JVMParentInstruction 
    with    JVMCompareAndJumpInstruction {
  def hasSideEffect = true

  def prettyPrint = "if_icmpgt " + branchTarget.prettyPrint

  def getTarget = branchTarget
}

case class JVMFCmpG() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fcmpg"
}

case class JVMFCmpL() extends JVMBinaryInstruction {
  def hasSideEffect = false

  def prettyPrint = "fcmpl"
}

sealed trait JVMDirective extends JVMInstruction {
  def stackEffect = 0
}

case class LocalsLimitDirective(var lim: Int) extends JVMDirective {
  def hasSideEffect = true

  def prettyPrint = ".limit locals " + lim.toString
}

case class StackLimitDirective(var lim: Int) extends JVMDirective {
  def hasSideEffect = false

  def prettyPrint = ".limit stack " + lim.toString
}

case class StackPopDirective(var lim: String) extends JVMDirective {
  def hasSideEffect = false

  def prettyPrint = ".stack " + lim
}

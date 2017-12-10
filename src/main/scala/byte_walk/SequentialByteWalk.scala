package byte_walk

import byteR._

class SequentialByteWalk[T] {
  def walk(item: T, jvmClass: JVMClass): Unit = {
    jvmClass.superClass.map(walk(item, _))
    jvmClass.fields.foreach(walk(item, _))
    jvmClass.methods.foreach(walk(item, _))
  }

  def walk(item: T, jvmClassRef: JVMClassRef): Unit = {
    // Nothing to be done on a class ref
  }

  def walk(item: T, jvmMethod: JVMMethod): Unit = jvmMethod match {
    case JVMMethod(name, arguments, result, body, isStatic) => {
      arguments.foreach(walk(item, _))
      walk(item, result)
      body.foreach(walk(item, _))
    }
  }

  def walk(item: T, jvmMethodRef: JVMMethodRef): Unit = jvmMethodRef match {
    case JVMMethodRef(classRef, name, argType, refType) => {
      walk(item, classRef)
      argType.foreach(walk(item, _))
      walk(item, refType)
    }
  }

  def walk(item: T, jvmField: JVMField): Unit = jvmField match {
    case JVMField(name, typ, isStatic) =>
      walk(item, typ)
  }

  def walk(item: T, jvmType: JVMType): Unit = jvmType match {
    case JVMArrayType(subType) => walk(item, subType)
    case JVMClassType(classRef) => walk(item, classRef)
  }

  def walk(item: T, jvmLabel: JVMLabel): Unit = {
    // Nothing to be done on a label.
  }

  def walk(item: T, jvmInstruction: JVMInstruction): Unit =
    jvmInstruction match {
      case parentInstruction : JVMParentInstruction =>
        parentInstruction match {
          case JVMCheckCast(ref) => walk(item, ref)
          case cmpAndJump : JVMCompareAndJumpInstruction =>
            walk(item, cmpAndJump.getTarget)
          case JVMLabelMark(label) => walk(item, label)
          case JVMJump(label) => walk(item, label)
          case JVMNew(classRef) => walk(item, classRef)
          case JVMPutStaticField(jvmClass, name, typ) => {
            walk(item, jvmClass)
            walk(item, typ)
          }
          case JVMGetStaticField(jvmClass, name, typ) => {
            walk(item, jvmClass)
            walk(item, typ)
          }
          case JVMPutField(jvmClass, name, typ) => {
            walk(item, jvmClass)
            walk(item, typ)
          }
          case JVMGetField(jvmClass, name, typ) => {
            walk(item, jvmClass)
            walk(item, typ)
          }
          case JVMInvokeStaticMethod(methodRef) =>
            walk(item, methodRef)
          case JVMInvokeVirtualMethod(methodRef) =>
            walk(item, methodRef)
          case JVMInvokeSpecialMethod(methodRef) =>
            walk(item, methodRef)
        }
        case other =>
    }
}

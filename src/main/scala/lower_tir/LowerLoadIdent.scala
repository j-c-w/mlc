package lower_tir

import exceptions.ICE
import byteR._
import tir._
import stdlib.StandardLibraries

object LowerLoadIdent {
  /* This class only deals with 'normal' identifiers.  All special cases are
   * expected to be handled in LowerExp.
   *
   * The expception is if the frontend has been changed to handle 'op'.
   *
   * In that case, this should be changed to generate method refs
   * for the appropriate functions.
   */
  def apply(exp: TIdent, env: TTypeEnv): List[JVMInstruction] = exp match {
    case ident: TNamedIdent => ident match {
      case TIdentVar(_) => throw new ICE("""Unexpected TIdent var during
        |lower_tir: %s""".stripMargin.format(ident.prettyPrint))
      case TInternalIdentVar(_) =>
        throw new ICE("Cannot lower internal ident var")
      case TIdentLongVar(names) =>
        StandardLibraries.loadExpressionFor(names)
      case ident @ TArgumentNode(funName, number) => List(
          JVMSelfLoad(),
          JVMGetField(
            JVMClassRef.classRefFor(LowerShared.functionAccessMap(funName)),
                                    "arg" + number,
                                    LowerType(env.getOrFail(ident)))
        )
      // Add 1 since 0 is reserved for the self reference.
      case TNumberedIdentVar(_, number) => List(JVMLocalALoad(number + 1))
      case ident @ TTopLevelIdent(name) => env.getOrFail(ident) match {
        case TFunctionType(_, _) => // This is a function, so it should be
          // loaded by creating a new class reference.
          // We may assume that this is Fun0.  All other functions
          // will not make direct accesses to the variables.
          List(JVMNew(JVMClassRef.classRefFor(LowerName("Fun0" + name))),
               JVMDup(),
          // Since we are loading a class for the first time, it should have
          // an empty constructor
               JVMInvokeSpecialMethod(
                 new JVMMethodRef(
                   JVMClassRef.classRefFor(LowerName("Fun0" + name)),
                   "<init>", List(), JVMVoidPrimitiveType())))
        case other =>
          List(JVMGetStaticField(JVMMainClassRef(), LowerName(name),
                                 LowerType(env.getOrFail(ident))))
      }
    }
    case TUnitIdent() =>
      List(new JVMNew(new JVMUnitRef()),
           new JVMDup(),
           new JVMInvokeSpecialMethod(
             new JVMMethodRef(new JVMUnitRef(), "<init>",
                              List(), new JVMVoidPrimitiveType())))
    case TIdentMatchError() =>
      List(new JVMNew(new JVMMatchExceptionRef()),
           new JVMDup(),
           new JVMInvokeSpecialMethod(
             new JVMMethodRef(new JVMMatchExceptionRef(), "<init>",
                              List(), JVMVoidPrimitiveType())))
    case TEmptyListIdent() => List(
    JVMNew(JVMLinkedListNilRef()),
    JVMDup(),
    JVMInvokeSpecialMethod(
      new JVMMethodRef(new JVMLinkedListNilRef(), "<init>",
                       List(), JVMVoidPrimitiveType())))
    case other => throw new ICE("Cannot load ident " + other.prettyPrint)
  }
}
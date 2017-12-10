package lower_tir

import exceptions.ICE
import byteR._
import tir._

object LowerStoreIdent {
  /* This class only deals with 'normal' identifiers.  All special cases are
   * expected to be handled in LowerExp.
   *
   * The expception is if the frontend has been changed to handle 'op'.
   *
   * In that case, this should be changed to generate method refs
   * for the appropriate functions.
   *
   * This is for generating sequences of instructions to store the top
   * of stack into an identifier.
   */
  def apply(exp: TIdent, env: TTypeEnv): List[JVMInstruction] = exp match {
    case ident: TNamedIdent => ident match {
      case TIdentVar(_) => throw new ICE("""Unexpected TIdent var during
        |lower_tir: %s""".stripMargin.format(ident))
      case TIdentLongVar(names) => throw new ICE("""Unexpected store to a long
        |ident""".stripMargin)
      case TInternalIdentVar(_) =>
        throw new ICE("Can't store to an internal ident")
      // Add 1 since 0 is reserved for the self reference.
      case TNumberedIdentVar(funName, number) =>
        List(JVMLocalAStore(number + 1))
      case TArgumentNode(funName, number) =>
        throw new ICE("""Store to a TArgument node.  There is no particular
          |reason that this has to be disallowed.  However, it should be done
          |with careful assesment on the performance impact of accessing
          |heap variables repeatedly.  See the implmentation in LowerLoadIdent
          |to implement this""".stripMargin)
      case TTopLevelIdent(name) =>
        List(JVMPutStaticField(JVMMainClassRef(), LowerName(name),
                               LowerType(env.getOrFail(ident))))
    }
    case other =>
      throw new ICE("Cannot store to identifier " + other.prettyPrint)
  }
}

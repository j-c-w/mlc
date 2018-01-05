package tir

import exceptions.ICE
import toplev.GenericTypeEnv
import tpass.TPass
import typecheck.TypeVariableGenerator

/*
 * This is a type environment for the TIR representation.
 *
 * It maps from TIdent to TType, and provides some auxillary
 * methods.
 *
 */

class TTypeEnv(parent: Option[TTypeEnv])
    extends GenericTypeEnv[TTypeEnv, TIdent, TType](parent) {
  def this() = this(None)

  /* This gets the type of an identifier that may by itself
   * not have a type (e.g. a tuple) and returns it
   * or fails in the process.
   */
  def compoundTypeOf(ident: TIdent): TType = ident match {
    case TIdentTuple(subTypes) => TTupleType(subTypes.map(compoundTypeOf(_)))
    case TUnderscoreIdent() => TypeVariableGenerator.getTVar()
    case other => super.getOrFail(other)
  }

  /* Given some function identifier with at least N curries, return
   * the first N curried arguments as a list.
   */
  def getNCurriedTypesFrom(n: Int, ident: TNamedIdent) =
    extractNCurriedTypesFrom(n, super.getOrFail(ident))

  private def extractNCurriedTypesFrom(n: Int, typ: TType): List[TType] =
    (n, typ) match {
      case (0, _) => List[TType]()
      case (n, TFunctionType(from, to)) =>
        from :: extractNCurriedTypesFrom(n - 1, to)
      case (n, other) =>
        throw new ICE("""Function assumed to have more curried arguments than it
                      | actually has""".stripMargin)
    }
}

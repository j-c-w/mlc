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
   * the Nth argument.
   */
  def getNthResultFrom(n: Int, ident: TNamedIdent) =
    extractNCurriedTypesFrom(n, super.getOrFail(ident))._2

  /* Given some function identifier with at least N curries, return
   * the first N curried arguments as a list.
   */
  def getNCurriedTypesFrom(n: Int, ident: TNamedIdent) =
    extractNCurriedTypesFrom(n, super.getOrFail(ident))._1

  /* Given some function with at least N curries, return the first
   * N curred arguments as a list and the last curry as an element.
   */
  def splitFunctionAt(n: Int, ident: TNamedIdent) =
    extractNCurriedTypesFrom(n, super.getOrFail(ident))

  private def extractNCurriedTypesFrom(n: Int,
                                       typ: TType): (List[TType], TType) =
    (n, typ) match {
      case (0, typ) => (List[TType](), typ)
      case (n, TFunctionType(from, to)) => {
        val (curriesList, res) = extractNCurriedTypesFrom(n - 1, to)
        (from :: curriesList, res)
      }
      case (n, other) =>
        throw new ICE("""Function assumed to have more curried arguments than it
                      | actually has""".stripMargin)
    }
}

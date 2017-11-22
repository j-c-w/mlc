package tir

import toplev.GenericTypeEnv
import tpass.TPass

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
    case other => super.getOrFail(other)
  }
}

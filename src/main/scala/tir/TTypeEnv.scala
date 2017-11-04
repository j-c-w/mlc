package tir

import toplev.GenericTypeEnv

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
}

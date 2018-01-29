package def_finder

import exceptions.ICE
import tir._
import tpass.TPass

class DefFinderWalk(ident: TNamedIdent)
    extends TPass[TTypeEnv, List[(TTypeEnv, TDec)]] {
  def combine(x: List[(TTypeEnv, TDec)], y: List[(TTypeEnv, TDec)]) =
    x ::: y

  def default = List()

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case TFun(name, rhs) =>
      if (name == ident) {
        List((env, dec))
      } else {
        super.apply(env, dec)
      }
    case TVal(name, rhs) => {
      if (name.getDeclaredIdents.contains(ident)) {
        List((env, dec))
      } else {
        super.apply(env, dec)
      }
    }
    case TJavaFun(name, curriedArgs, rhs, jEnv) =>
      if (name == ident) {
        List((env, dec))
      } else {
        super.apply(env, dec)
      }
  }
}

package def_finder

import exceptions.ICE
import tir._
import tpass.TPass

class DefFinderWalk(ident: TNamedIdent)
    extends TPass[TTypeEnv, Option[(TTypeEnv, TDec)]] {
  def combine(x: Option[(TTypeEnv, TDec)], y: Option[(TTypeEnv, TDec)]) =
    (x, y) match {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(y)) => Some(y)
      case (Some(x), Some(y)) => throw new ICE("""Error: Found a duplicate
        |definition, both %s and %s are definitons of the same identifier.""".
        stripMargin.format(x._2.prettyPrint, y._2.prettyPrint))
    }

  def default = None

  override def apply(env: TTypeEnv, dec: TDec) = dec match {
    case TFun(name, rhs) =>
      if (name == ident) {
        Some((env, dec))
      } else {
        super.apply(env, dec)
      }
    case TVal(name, rhs) => {
      if (name.getDeclaredIdents.contains(ident)) {
        Some((env, dec))
      } else {
        super.apply(env, dec)
      }
    }
    case TJavaFun(name, curriedArgs, rhs, jEnv) =>
      if (name == ident) {
        Some((env, dec))
      } else {
        super.apply(env, dec)
      }
  }
}

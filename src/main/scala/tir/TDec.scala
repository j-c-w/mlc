package tir

import toplev.GenericPrintable

trait TDec extends TWalkable with GenericPrintable

case class TFun(var typ: TType, var patterns: List[TPat],
                var expressions: List[TExp]) extends TDec {
  def prettyPrint = """
  fun: %s
    %s """.format(typ.prettyPrint, (patterns zip expressions).map {
      case (pat, exp) => pat.prettyPrint + " => " + exp.prettyPrint
    }.mkString("\n    | "))

  def walk(f: TPass) = 
    if (f(this)) {
      typ.walk(f)
      patterns.foreach(_.walk(f))
      expressions.foreach(_.walk(f))
    }
}

case class TVal(var typ: TType, var ident: TIdent,
                var exp: TExp) extends TDec {
  def prettyPrint = """
  val :s (%s) = %s
  """.format(typ.prettyPrint, ident.prettyPrint, exp.prettyPrint)

  def walk(f: TPass) = 
    if (f(this)) {
      typ.walk(f)
      ident.walk(f)
      exp.walk(f)
    }
}

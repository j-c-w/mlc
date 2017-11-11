package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends TWalkable with GenericPrintable

case class TFun(var name: TIdentVar, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  fun %s
    %s """.format(name.prettyPrint,
                  patterns.map(_.prettyPrint).mkString("\n    | "))

  def walk[T](item: T, f: TPass[T]) = 
    if (f(item, this)) {
      name.walk(item, f)
      patterns.foreach(_.walk(item, f))
    }
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  val %s = %s
  """.format(ident.prettyPrint, exp.prettyPrint)

  def walk[T](item: T, f: TPass[T]) = 
    if (f(item, this)) {
      ident.walk(item, f)
      exp.walk(item, f)
    }
}

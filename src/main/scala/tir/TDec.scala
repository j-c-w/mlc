package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends GenericPrintable

case class TFun(var name: TIdentVar, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  fun %s
    %s """.format(name.prettyPrint,
                  patterns.map(_.prettyPrint).mkString("\n    | "))
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  val %s = %s
  """.format(ident.prettyPrint, exp.prettyPrint)
}

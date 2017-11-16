package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends GenericPrintable

case class TFun(var name: TIdentVar, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  |fun %s
  |  %s
  |""".stripMargin.format(name.prettyPrint,
                          patterns.map(_.prettyPrint).mkString("\n    | "))
}

/* This is inserted by the let lowering class. */
case class TJavaFun(var name: TIdentVar, var cases: List[TExpFunLetMatchRow])
    extends TDec {
  def prettyPrint = """
  |fun %s =
  |%s
  """.stripMargin.format(name.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n    |"))
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  |val %s = %s
  """.stripMargin.format(ident.prettyPrint, exp.prettyPrint)
}

package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends GenericPrintable

case class TFun(var name: TIdentVar, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  |fun %s
  |%s
  |""".stripMargin.format(name.prettyPrint,
                          patterns.map(_.prettyPrint).mkString("\n    | "))
}

/* This is inserted by the let lowering pass. */
case class TJavaFun(var name: TIdentVar, var exp: TExpFunLet, env: TTypeEnv)
    extends TDec {
  def prettyPrint = """
  |fun %s =
  |%s
  """.stripMargin.format(name.prettyPrint,
                         exp.prettyPrint)
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  |val %s = %s
  """.stripMargin.format(ident.prettyPrint, exp.prettyPrint)
}

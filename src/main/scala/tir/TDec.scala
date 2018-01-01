package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends TTree {
  def nodeClone: TDec
}

case class TFun(var name: TNamedIdent, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  |fun %s
  |%s
  |""".stripMargin.format(name.prettyPrint,
                          patterns.map(_.prettyPrint).mkString("\n    | "))

  def curriedArgumentsCount: Int =
    patterns(0).pat.length

  def nodeClone =
    new TFun(name.nodeClone, patterns.map(_.nodeClone))
}

/* This is inserted by the let lowering pass. */
case class TJavaFun(var name: TTopLevelIdent,
                    var curriedArgs: List[TInternalIdentVar],
                    var exp: TExpFunLet, env: TTypeEnv)
    extends TDec {
  def prettyPrint = """
  |fun %s =
  |%s
  """.stripMargin.format(name.prettyPrint,
                         exp.prettyPrint)

  def nodeClone =
    new TJavaFun(name.nodeClone, curriedArgs.map(_.nodeClone),
                 exp.nodeClone, env)
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  |val %s = %s
  """.stripMargin.format(ident.prettyPrint, exp.prettyPrint)

  def nodeClone =
    new TVal(ident.nodeClone, exp.nodeClone)
}

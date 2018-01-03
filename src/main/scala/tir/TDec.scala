package tir

import toplev.GenericPrintable
import tpass.TPass

sealed trait TDec extends TTree {
  def nodeClone(env: TTypeEnv): TDec
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

  def nodeClone(env: TTypeEnv) =
    new TFun(name.nodeClone(env), patterns.map(_.nodeClone(env)))
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

  def nodeClone(env: TTypeEnv) =
    new TJavaFun(name.nodeClone(env), curriedArgs.map(_.nodeClone(env)),
                 exp.nodeClone(env), env)
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  |val %s = %s
  """.stripMargin.format(ident.prettyPrint, exp.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TVal(ident.nodeClone(env), exp.nodeClone(env))
}

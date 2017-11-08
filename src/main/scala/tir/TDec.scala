package tir

import toplev.GenericPrintable

sealed trait TDec extends TWalkable with GenericPrintable

case class TFun(var name: TIdentVar, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  fun %s
    %s """.format(name.prettyPrint,
                  patterns.map(_.prettyPrint).mkString("\n    | "))

  def walk(env: TTypeEnv, f: TPass) = 
    if (f(env, this)) {
      name.walk(env, f)
      patterns.foreach(_.walk(env, f))
    }
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  val %s = %s
  """.format(ident.prettyPrint, exp.prettyPrint)

  def walk(env: TTypeEnv, f: TPass) = 
    if (f(env, this)) {
      ident.walk(env, f)
      exp.walk(env, f)
    }
}

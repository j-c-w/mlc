package tir

import toplev.GenericPrintable

sealed trait TDec extends TWalkable with GenericPrintable

case class TFun(var name: TIdent, var patterns: List[TExpMatchRow])
    extends TDec {
  def prettyPrint = """
  fun %s
    %s """.format(name.prettyPrint,
                  patterns.map(_.prettyPrint).mkString("\n    | "))

  def walk(f: TPass) = 
    if (f(this)) {
      name.walk(f)
      patterns.foreach(_.walk(f))
    }
}

case class TVal(var ident: TIdent, var exp: TExp) extends TDec {
  def prettyPrint = """
  val %s = %s
  """.format(ident.prettyPrint, exp.prettyPrint)

  def walk(f: TPass) = 
    if (f(this)) {
      ident.walk(f)
      exp.walk(f)
    }
}
